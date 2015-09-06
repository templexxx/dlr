; 计算nn~n的阶乘
(define fact
  (lambda(nn n)
    (let loop ((n1 n) (p n))
      (if (= n1 nn)
          p
          (let((m (- n1 1)))
            (loop m (* m p)))))))

; 计算排列组合
(define how_many
  ; T中取H
  (lambda(H T)
    (let((t1(- T H)))
      (let((t2(+ t1 1)))
        (/ (fact t2 T) (fact 1 H))))))

; 定义单位磁盘集群，单位时间
; 按照3%的年故障率来计算（Backblaze数据，与经验值也基本吻合），28800块磁盘每10小时将坏掉1块
; 10小时则为平均修复时间（4T SATA）,28800在这里与10小时建立了等价关系
; 倘若修复时间（单位时间）缩短1倍，如5小时，则将N_unit修改为（* 28800 2），因为57600块磁盘5每小时会坏一块
; 倘若磁盘故障率缩小1倍，如1.5%，则则将N_unit修改为（* 28800 2），即57600块磁盘10小时会坏一块

(define N_unit 28800)

; 计算单位时间内小于R块盘损坏的概率
(define r_broken
  ; N为磁盘总数 考虑到可执行的计算范围，N限定在3456000及以下，大约3EB的数据量
  ; 同时过小的规模没有统计意义，因此N的下限为28800
  (lambda(R N)
    ; 单位时间平均要坏的盘数
      (let((n1(/ N N_unit)))
        (let((p0(exp  (/ n1 -1))))
          (let((R0(- R 1)))
        (let loop((r 0) (p p0))
          (if (eq? r R0)
              p
              (let((m(+ r 1)))
                (let((pn(/ (* (expt n1 m)
                             (exp  (/ n1 -1)))
                           (fact 1 m))))
                  (loop m (+ p pn)))))))))))

;计算单位时间内很大可能坏几块盘以上（90%以上几率坏R及以上块盘）
(define q_broken
  (lambda(N)
    (let((n1(/ N N_unit)))
      (let((r1(r_broken n1 N)))
        (let((q1(- 1 r1)))
      (let loop((n2 n1)(q q1))
        (if (<= q 0.1)
            n2
            (let((n3(- n2 1)))
              (loop n3 (r_broken n3 N))))))))))

; 计算坏R块盘的概率
(define poisson
  ; n1为28800的倍数
  (lambda(n1 R)
    (cond ((> R 145)
           (/ (* (expt n1 R)
                 (inexact->exact (exp (/ n1 -1))))
              (fact 1 R)))
          (else
           (/ (* (expt n1 R)
          (exp (/ n1 -1)))
       (fact 1 R))))))

; 计算坏掉的R块盘中，恰巧能组成多少个副本组合
; 副本数为C
(define all_dead
  (lambda(R C N)
    (/ (* (how_many C R)
       (/ N C))
       (how_many C N))))

; 三副本时坏掉R块盘，恰能组成的副本组合数
(define all_dead3
  (lambda(R N)
    (/ (/ (* (* (- R 2) (- R 1)) R) 
          (* (- N 2) (- N 1))) 3)))

; 开三次方根
(define cube_root
  (lambda(x)
    (exp (/ (log x) 3))))

; 三副本方案下，求100%丢失一组副本组合至少需要多少块盘
(define 3_loss_need
  (lambda(N)
    (let((d(/ (* (* 3 (- N 2)) (- N 1)) -1)))
      (let((d1(/ (+ 3 d) -2)))
        (let((d2(expt d1 2)))
          (let((d3(sqrt (- d2 (/ 1 27)))))
      (round (+ 1 (+ 1 (+ (cube_root (+ d1 d3))
                          (cube_root (- d1 d3))))))))))))

; 计算三副本情况下最低的可靠性
; 将 all_dead3（三副本）与poisson相乘，可得关于R的方程,且在R=n1+2处有最大值
; 考虑高位数浮点数运算产生的误差，这个最大值可能将会出现在n1+2附近一两位数

(define 3min_durable
  (lambda(N)
    (let((n1(/ N N_unit)))
      (let((R (+ n1 2)))
        (* (all_dead3 R N) (poisson n1 R))))))

; 计算多副本情况下的最低可靠性
; 将all_dead与poisson相乘，可得关于R的方程，且在R=n1-1+C处有最大值（C为副本数）
; 考虑高位数浮点数运算产生的误差，这个最大值可能将会出现在n1-1+C附近一两位数

(define cmin_durable
  (lambda(N C)
    (let((n1(/ N N_unit)))
      (let((R (- (+ n1 C) 1)))
       (* (all_dead R C N) (poisson n1 R))))))

; 计算最可能出现的情况
; 须要排除R小于C的情况，否则计算没有意义
; 对poisson做演化，可以得到最大值将出现在n1-1处
; 考虑高位数浮点数运算产生的误差，这个最大值可能将会出现在n1-1附近一两位数
; N >= 4* 28800

(define c_durable
  (lambda(N C)
    (let((n1(/ N N_unit)))
      (let((R(- n1 1)))
        (* (all_dead R C N) (poisson n1 R))))))



  
     

                             


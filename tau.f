program taub

real rfric, drittl,ireib,rr,rcd,grav,uv,hzg

rfric=10.
          drittl=1./3.
          if(ireib.eq.0) then
                rr = 0.
                rcd = 0.
          else if(ireib.eq.1) then
                rr = rfric
                rcd = 0.
                if( uv > 0. ) rcd = rr*hzg*hzg/uv
          else if(ireib.eq.2) then              ! Strickler
                rcd = grav/((rfric**2)*(hzg**drittl))
                rr = rcd*uv/(hzg*hzg)
          else if(ireib.eq.3) then              ! Chezy
                rcd = grav/(rfric**2)
                rr = rcd*uv/(hzg*hzg)
          else if(ireib.eq.4) then
                rr = rfric/hzg
                rcd = 0.
                if( uv > 0. ) rcd = rr*hzg*hzg/uv
          else if(ireib.eq.5) then              ! constant drag coefficient
                rcd = rfric
                rr = rcd*uv/(hzg*hzg)
          else if(ireib.eq.6) then              ! rfric is z0
                rcd = cdf(hzg,rfric)
                rr = rcd*uv/(hzg*hzg)
          else if(ireib.eq.7) then              ! mixed Strickler / drag
                if( rfric .ge. 1. ) then
                  rcd = grav/((rfric**2)*(hzg**drittl))
                else
                  rcd = rfric
                end if
                rr = rcd*uv/(hzg*hzg)
          else if(ireib.eq.8) then              ! use z0 computed by sedtrans
                ss = 0.
                do ii=1,3
                  k = nen3v(ii,ie)
                  ss = ss + z0bk(k)
                end do
                ss = ss / 3.
                rcd = cdf(hzg,ss)
                rr = rcd*uv/(hzg*hzg)
          else if(ireib.eq.9) then              ! function of fluid mud (AR:)
                ss = 0.
                do ii=1,3
                  k = nen3v(ii,ie)
                  lmax = ilhkv(k)
                  call set_mud_roughness(k,lmax,alpha) ! (ARON)
                  ss = ss + alpha * rfric ! rfric = ks for this parameterization
                end do
                ss = ss / 3.
                z0bk(k) = ss
                !z0bk(k) = max(z0bkmud(k),ss)
                !ss = rfric     !ARON: do you really need to compute ss above?
                rcd = cdf(hzg,ss)
                rr = rcd*uv/(hzg*hzg)
         endif
         uuvv = uv / hzg
         bnstressv(ie) = rcd * uuvv * uuvv

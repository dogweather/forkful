---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Python: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜 
날짜를 미래나 과거로 계산하는 데 관심이 있을 수 있는 이유는 크게 두 가지가 있습니다. 첫째, 생일이나 결혼 기념일 같은 특별한 날짜를 기록하고 싶을 때 혹은 미래에 일어날 이벤트를 미리 계획하기 위해 사용할 수 있습니다. 둘째, 비즈니스 분석이나 데이터 처리를 위해 날짜를 기반으로 한 시간 간격을 계산할 수 있습니다.

## 어떻게

날짜를 미래나 과거로 계산하는 것은 `timedelta` 객체를 사용하여 쉽게 할 수 있습니다. 아래는 10월 31일에서 100일 후의 날짜를 계산하는 예시입니다.
```python
# 필요한 라이브러리 import하기
from datetime import date, timedelta

# 오늘 날짜를 변수에 저장
today = date.today()

# 100일 후의 날짜를 계산하여 변수에 저장
after_100_days = today + timedelta(days=100)

# 계산된 날짜를 출력
print(after_100_days)
# Output: 2020-02-08
```
위와 같은 방법으로 날짜 간격을 일, 주, 달, 년 등 다양한 형태로 계산할 수 있습니다. 또한 `strftime()` 함수를 이용하면 계산된 날짜를 원하는 형식으로 출력할 수 있습니다.

## 딥 다이브 
날짜를 계산할 때 알아두면 유용한 여러 가지 팁이 있습니다. 첫째, `timedelta` 객체에는 `days`, `seconds`, `microseconds` 등의 속성이 존재하는데, 이를 이용하여 각각 날짜, 초, 마이크로초 단위로 날짜 간격을 계산할 수 있습니다. 둘째, `datetime` 모듈에는 여러 가지 유용한 함수들이 존재하는데, 이 중 `date()`, `time()`, `datetime()` 등을 이용하면 날짜와 시간을 조합한 객체를 만들 수 있습니다. 셋째, 날짜 간격을 계산할 때 주의할 점은 윤년과 윤달이 있다는 점입니다. 이러한 특수한 경우에는 계산 결과가 예상과 다를 수 있으므로 이에 대한 예외 처리를 고려해야 합니다.

## 참고 자료
- [Python datetime 모듈 사용법](https://wikidocs.net/2188)
- [Python timedelta 객체 사용법](https://wikidocs.net/5243)
- [날짜 계산하기 - Naver D2](https://d2.naver.com/helloworld/1336)
- [Understanding Python's datetime module - PyYYC meetup](https://pyykankamocamp.cdn.ampproject.org/v/s/pyyykankamocamp.appspot.com/ja/pyyykankamocamp.appspot.com/s/understanding-pythons-datetime-module/amp/amp_ja/amp_py/amp_py/amp_py/amp1/amp-1stamplazy.php?v=tref_py_original_py_pyyyc_archive_l_hk_e_sc_ja_p002203233_hg_wap_gt_py_py_py_h_cp_sg1_amp_amp_yz=usg=afqjcneunmz3rn23xdya0yf-1dyue1t0bw_cp_a_sg_yamp_yi=yamp_yt=amp_yu=/pyrid-m124974-p124974-tsourceamp_x=ampo_cw=amp_x=_ed==QAV1Bdug2D15txe_0C_y-5CxA_9CH_nK32Cjamz_uF_1DMR-p-y9XooU1tMXWVKb_hN_poD_UgtclB
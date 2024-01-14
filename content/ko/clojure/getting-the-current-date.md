---
title:                "Clojure: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 왜
현재 날짜를 가져오는 것에 참여하려는 이유는 무엇인가요? 

현재 날짜를 얻는 것은 많은 프로그래밍에서 중요한 요소입니다. 나중에 사용하기 위해 날짜를 저장하는 것이 필요할 수 있기 때문입니다. 그리고 다양한 어플리케이션에서 날짜가 필요할 수 있고, 이를 통해 정보를 필터링하거나 정렬하는 등의 작업을 수행할 수 있습니다.

# 방법
늘 그렇듯이, Clojure (이북)에서 임포팅을해야합니다.
```Clojure
(require '[clojure.java-time :as t])
```

그리고 현재 날짜를 가져오려면 다음을 실행하면됩니다.
```Clojure
(def current-date (t/local-date))
```

출력은 다음과 같아야합니다.
"2021-05-30"

또한 현재 날짜는 다양한 형식으로 표시 할 수 있습니다. 예를 들어, 다음과 같이 한국 시간으로 표시 할 수 있습니다.
```Clojure
(def timezone (t/time-zone "Asia/Seoul"))
(def korean-date (t/local-date timezone))
```

출력은 다음과 같아야합니다.
"2021-05-30"

# 깊은 입문
날짜를 가져올뿐만 아니라 더 많은 작업도 가능합니다. 예제로, 다음은 현재 날짜와 비교하여 나중인지 여부를 확인하는 코드입니다.
```Clojure
(def future-date (t/next-day current-date))
(def is-future? (t/after? future-date current-date))
```

출력은 다음과 같아야합니다.
true

또한 날짜간의 차이도 확인할 수 있습니다.
```Clojure
(def another-date (t/local-date-of 2021 6 30))
(def difference (t/days-difference current-date another-date))
```

출력은 다음과 같아야합니다.
"31"

# 관련 자료
- https://github.com/dm3/clojure.java-time
- https://clojure.org/guides/date_and_time
- https://rosettacode.org/wiki/Unix/ls#Clojure
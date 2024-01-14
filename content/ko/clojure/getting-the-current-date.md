---
title:                "Clojure: 현재 날짜 가져오기"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜
현재 날짜를 얻는 것에 참여하는 이유는 다양합니다. 일반적으로 현재 날짜와 시간은 중요한 정보이며, 이 정보를 활용하여 신뢰성 있는 애플리케이션을 만들 수 있습니다.

## 어떻게
Clojure에서 현재 날짜를 얻는 것은 매우 간단합니다.`java.util.Date` 클래스에 대한 참조를 가져와서 `now` 함수를 사용해 현재 시간을 가져올 수 있습니다. 여기에 예제 코드와 출력을 제공합니다.

```Clojure
; Clojure 코드
(def now (java.util.Date.))

; 출력 예시
#inst "2020-06-07T19:34:30.585-00:00"
```

우리는 현재 날짜를 형식화 할 수도 있습니다. 다음 예시를 확인해보세요.

```Clojure
; Clojure 코드
(require '[java.time :as time])

(def date-time (time/local-date-time))

(time/format date-time "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")

; 출력 예시
"2020-06-07T19:34:30.585Z"
```

## 깊이 살펴보기
현재 날짜를 얻는 데는 다양한 방법이 있습니다. 위의 예제에서 우리는 `java.util.Date` 클래스와 `java.time` 네임스페이스를 사용했습니다. 또한 Clojure는 `clj-time` 라이브러리와 `clj-time.coerce` 네임스페이스도 제공합니다. 이를 사용하여 원하는 형식으로 날짜를 변환할 수 있습니다.

## 참고
- Clojure 공식 문서: https://clojure.org/
- `java.util.Date` 레퍼런스: https://docs.oracle.com/javase/8/docs/api/java/util/Date.html
- `java.time` 네임스페이스: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- `clj-time` 라이브러리: https://github.com/clj-time/clj-time
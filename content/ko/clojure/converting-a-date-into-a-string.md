---
title:    "Clojure: 날짜를 문자열로 변환하기"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 왜

코드에 포함된 날짜를 문자열로 변환하는 이유는 데이터를 가공하고 분석하기 위해서입니다.

## 어떻게

```Clojure
;; 현재 시간을 가져옵니다.
(def now (java.util.Date.))

;; "EEE, MMM d, yyyy" 형식으로 날짜를 포매팅합니다.
(def formatted-date (java.text.SimpleDateFormat. "EEE, MMM d, yyyy"))

;; 날짜를 문자열로 변환합니다.
(formatted-date.format now)

;; "Wednesday, Oct 21, 2020" 을 출력합니다. 
```

## 깊게 들어가기

Clojure에서는 `java.util.Date`와 `java.text.SimpleDateFormat` 클래스를 사용하여 날짜를 문자열로 변환할 수 있습니다. `java.util.Date` 클래스는 날짜와 시간의 정보를 포함하고 있고, `java.text.SimpleDateFormat` 클래스는 날짜를 원하는 양식으로 포매팅할 수 있도록 도와줍니다. 또한, Clojure에서는 날짜를 다양한 포맷으로 변환할 수 있는 다른 라이브러리도 많이 제공하고 있으므로 필요에 따라 적합한 라이브러리를 선택하여 사용할 수 있습니다. 

## 참고

- [Clojure 기본 문서](https://clojure.org/)
- [Clojure 날짜 및 시간 라이브러리](https://github.com/ztellman/clj-time)
- [Clojure에서 날짜 포매팅하기](https://clojureal-dong.tistory.com/29)
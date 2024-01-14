---
title:                "Clojure: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 것은 프로그래머로서 중요한 기술입니다. 날짜를 문자열로 변환하는 이유는 형식이나 출력 방식을 변경할 수 있기 때문입니다.

## 사용 방법

날짜를 문자열로 변환하는 가장 간단한 방법은 `format` 함수를 사용하는 것입니다. 다음은 이 함수를 사용하는 예시입니다.

```Clojure
(format "오늘의 날짜는 %t" (java.util.Date.))
```

출력은 다음과 같이 나옵니다.

```
오늘의 날짜는 Tue Jan 01 00:00:00 UTC 2019
```

만약 원하는 형식이 다르다면, 다음과 같이 `date-time`라이브러리를 사용할 수 있습니다.

```Clojure
(require '[org.joda.time.DateTime :as dt])
(require '[clj-time.format :as f])

(f/unparse (f/formatters :date-time) (dt/now))
```

출력은 다음과 같이 나옵니다.

```
2019-01-01T00:00:00.000Z
```

## 깊게 들어가기

날짜를 문자열로 변환하는 또 다른 방법은 `java.text.SimpleDateFormat` 클래스를 사용하는 것입니다. 이 클래스를 사용하면 원하는 형식으로 날짜를 출력할 수 있습니다. 다음은 이 방법을 사용하는 예시입니다.

```Clojure
(let [now (java.util.Date.)
      formatter (java.text.SimpleDateFormat. "yyyy년 MM월 dd일 HH시 mm분 ss초")]
  (.format formatter now))
```

출력은 다음과 같이 나옵니다.

```
2019년 01월 01일 00시 00분 00초
```

## 참고

- [ClojureDocs - format](https://clojuredocs.org/clojure.core/format)
- [ClojureDocs - date-time](https://clojuredocs.org/clj-time.api/date-time)
- [Java(TM) Platform SE 7 API - SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하나?

날짜를 문자열로 변환하는 것은 일정한 표현식으로 표현된 날짜를 접근하고 가공하기 위한 기법입니다. 이는 파일 이름을 시간 관련 정보와 함께 생성하거나, 사용자에게 날짜 정보를 표시하는 등 다양한 경우에 활용됩니다.
 

## 어떻게 사용하나?

Clojure에서는 `java.text.SimpleDateFormat` 클래스를 이용하여 날짜를 문자열로 쉽게 변환할 수 있습니다.

```Clojure
(import 'java.text.SimpleDateFormat)
(import 'java.util.Date)

(defn date-to-string [date format]
  (let [formatter (SimpleDateFormat. format)]
    (.format formatter date)))

(println (date-to-string (Date.) "yyyy-MM-dd HH:mm:ss")) ;; 예제 출력: 2021-12-03 18:20:25
```

## 깊은 이해

1. **역사적 배경:** 초기에는 날짜에 대한 형식을 직접 지정해야 했습니다. 이후 Java에서는 SimpleDateFormat 클래스를 제공해 날짜를 문자열로 쉽게 변환합니다.

2. **대안:** Clojure에서는 java.time 모듈을 사용하여 더 심화된 날짜 및 시간 작업을 수행할 수 있습니다. 

3. **구현 세부사항:** Clojure 프로그램에서는 Java 클래스를 직접 호출하여 작업을 수행합니다. 이 경우 `SimpleDateFormat` 클래스와 `format` 메서드를 사용하여 날짜를 특정 형식의 문자열로 변환합니다.

## 참고 자료

2. [Java SimpleDateFormat 문서](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
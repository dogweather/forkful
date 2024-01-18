---
title:                "문자열에서 날짜 분석하기"
html_title:           "Clojure: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
날짜를 문자열에서 파싱한다는 것은 무엇인지 그리고 프로그래머들이 왜 이것을 하는지를 두-세 문장으로 설명합니다.

날짜를 문자열에서 파싱한다는 것은 문자열에서 날짜 정보를 추출해내는 것을 의미합니다. 예를 들어, "2021년 9월 14일"이라는 문자열에서 "2021-09-14"와 같은 형식의 날짜 정보를 추출하는 것입니다. 프로그래머들은 이를 필요로 하는 이유는 다양합니다. 예를 들어, 웹 애플리케이션에서 사용자가 입력한 날짜를 처리해야 할 때 등이 그 이유에 해당합니다.

## 어떻게:
아래의 ```Clojure ... ``` 코드 블록에서 코딩 예제와 샘플 출력을 제공합니다.

```Clojure
(require '[java-time :as jt])

;; 문자열에서 날짜 정보를 추출하는 방법
(def str-date "2021년 9월 14일")
(jt/local-date (jt/parse "yyyy년 M월 d일" str-date)) ; => #object[java.time.LocalDate 0x301c4dc "2021-09-14"]

;; 현재 날짜 정보를 문자열로 포맷하는 방법
(jt/format "오늘은 yyyy년 M월 d일입니다." (jt/local-date)) ; => "오늘은 2021년 9월 14일입니다."
```

## 깊이 파고들기:
(1) 역사적인 맥락, (2) 대안들, (3) 날짜를 문자열에서 파싱하는 구현에 대한 자세한 정보 등을 제공합니다.

Java 8부터 추가된 java.time 라이브러리에서 제공하는 기능을 이용하면 쉽게 날짜를 문자열에서 파싱할 수 있습니다. 이전에는 SimpleDateFormat과 같은 클래스를 사용해야 했지만 java.time 라이브러리 덕분에 더 직관적이고 간편한 방법을 제공합니다.

## 더 알아보기:
관련 링크를 제공합니다.

- [java.time 문서](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [SimpleDateFormat과 java.time의 비교](https://stackoverflow.com/questions/33255581/simpledateformat-vs-java-time-which-one-is-the-best-for-your-need)
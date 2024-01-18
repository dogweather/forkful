---
title:                "문자열에서 날짜 추출하기"
html_title:           "Java: 문자열에서 날짜 추출하기"
simple_title:         "문자열에서 날짜 추출하기"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 뭐고 왜 하는 거야?
문자열에서 날짜를 분석하는 것은 프로그래머가 일반적으로 문자열 형태로 된 날짜를 컴퓨터가 이해할 수 있는 형식으로 변환하는 것을 의미합니다. 이 과정은 프로그래머가 날짜를 다루기 쉽게 만들어줍니다.

## 진행 방식:
```Java
// 문자열에서 날짜 분석 예제
String dateStr = "2020-04-17"; // 문자열 형태의 날짜
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd"); // 날짜 형식 지정
LocalDate date = LocalDate.parse(dateStr, formatter); // 문자열 날짜를 LocalDate 객체로 변환
System.out.println(date); // 변환된 날짜 출력
```
출력 결과: 2020-04-17

## 더 들어가보기:
(1) 과거의 맥락: 날짜 처리는 컴퓨터가 처음 개발되었을 때부터 중요한 과제였습니다. 날짜를 컴퓨터가 이해할 수 있는 형식으로 변환하는 것은 오랜 역사를 가지고 있습니다. (2) 대안: 자바의 더 오래된 버전에서는 SimpleDateFormat 클래스를 사용하여 문자열 날짜를 분석하는 방법이 있었지만 Java 8부터는 java.time 패키지에서 제공하는 새로운 API를 사용하는 것이 권장됩니다. (3) 구현 세부 사항: java.time 패키지의 LocalDate 클래스를 사용하여 문자열 날짜를 LocalDate 객체로 변환합니다. 가장 중요한 부분은 날짜 형식을 지정하는 DateTimeFormatter 클래스입니다.

## 참고자료:
- [Java 8 날짜와 시간 API 문서](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [SimpleDateFormat 클래스 문서](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
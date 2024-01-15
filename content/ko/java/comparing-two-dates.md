---
title:                "두 날짜 비교하기"
html_title:           "Java: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜
날짜 비교를 하는 것이 왜 중요한지 알려드리겠습니다.

## 어떻게
날짜를 비교하는 방법을 샘플 코드와 함께 알려드리겠습니다.

```Java
// 두 개의 날짜 객체 생성
LocalDate date1 = LocalDate.of(2021, 5, 15);
LocalDate date2 = LocalDate.of(2020, 3, 25);

// 비교 연산자를 통해 날짜 비교
boolean isAfter = date1.isAfter(date2);
boolean isBefore = date1.isBefore(date2);
boolean isEqual = date1.isEqual(date2);

// 결과 출력
System.out.println(isAfter); // true
System.out.println(isBefore); // false
System.out.println(isEqual); // false
```

### 결과
- `isAfter`: 두 번째 날짜가 첫 번째 날짜보다 이후인지 여부를 반환합니다.
- `isBefore`: 두 번째 날짜가 첫 번째 날짜보다 이전인지 여부를 반환합니다.
- `isEqual`: 두 날짜가 같은지 여부를 반환합니다.

## 딥 다이브
날짜를 비교하는 데는 여러 가지 방법이 있습니다. `java.time` 패키지의 `LocalDate` 클래스를 이용하면 날짜를 간단하게 생성하고 비교할 수 있습니다. 또한, `date1.compareTo(date2)` 메소드를 통해 두 날짜를 정확히 비교할 수도 있습니다. 날짜 비교를 할 때는 날짜형식에 주의하여야 합니다. 예를 들어, `LocalDate`의 경우 연도-월-일 순서로 입력해야 올바른 비교가 가능합니다.

## 더 알아보기
- [Java LocalDate 클래스 가이드](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java Date and Time API 가이드](https://docs.oracle.com/javase/8/docs/technotes/guides/datetime/index.html)
- [Java 날짜 비교하기 (compareTo() 메소드 이용)](https://www.codejava.net/java-core/the-java-language/numbers/how-to-compare-dates-in-java-using-comparable-and-compareto-methods)
- [Java 비교 연산자를 이용한 날짜 비교](https://javarevisited.blogspot.com/2015/06/how-to-compare-two-dates-in-java-example.html)
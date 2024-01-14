---
title:    "Java: 현재 날짜 얻기"
keywords: ["Java"]
---

{{< edit_this_page >}}

## 왜
현재 날짜를 가져오는 것에 참여하는 이유는 단순합니다. 현재 날짜는 프로그래밍에서 자주 사용되며 예약된 작업, 파일 및 이벤트의 생성 날짜/시간을 결정하는 데 유용합니다. 또한 사용자에게 더 나은 사용자 경험을 제공하기 위해 화면과 관련된 날짜를 표시하는 등 다양한 용도로 사용됩니다.

## 방법
자바에서 현재 날짜를 가져오는 방법은 매우 간단합니다. 다음 코드 블록을 참조하십시오.

```java
// 현재 날짜 가져오기
LocalDate today = LocalDate.now();

// 현재 시간 가져오기
LocalTime currentTime = LocalTime.now();

// 현재 날짜 및 시간 가져오기
LocalDateTime currentDateTime = LocalDateTime.now();

// 원하는 형식으로 날짜 및 시간 출력
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy/MM/dd HH:mm:ss");
String formattedDateTime = currentDateTime.format(formatter);
System.out.println(formattedDateTime);
```

위의 예제에서는 `LocalDate`, `LocalTime`, `LocalDateTime` 및 `DateTimeFormatter` 클래스를 사용하여 현재 날짜 및 시간을 가져와 원하는 형식으로 출력하는 방법을 보여줍니다. 이 코드를 실행하면 출력 결과는 `2021/04/30 11:25:15`와 같이 현재 날짜 및 시간을 나타냅니다.

## 깊이 들어가기
`LocalDate`, `LocalTime`, `LocalDateTime` 및 `DateTimeFormatter` 클래스는 자바 8부터 추가된 `java.time` 패키지에 포함되어 있습니다. 이들은 모두 불변(immutable) 클래스로, 동시성(Concurrency) 문제가 발생하지 않도록 설계되었습니다.

또한, 이들 클래스는 기존 `Date` 및 `Calendar` 클래스보다 훨씬 간편하고 유연한 방식으로 날짜 및 시간을 다룰 수 있습니다. 예를 들어, `LocalDateTime` 클래스는 `Date` 클래스의 단순한 날짜와 시간 정보만 저장하는 반면, `LocalDate` 및 `LocalTime` 클래스는 날짜와 시간 정보를 따로 저장하여 세부적인 조작이 가능합니다.

이러한 클래스들의 메소드는 매우 직관적이며, Javadoc 문서를 통해 더 자세한 정보를 얻을 수 있습니다.

## 참고
- <https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html>
- <https://www.baeldung.com/java-8-date-time-intro>
- <https://mkyong.com/java8/java-8-how-to-convert-string-to-localdate/>
- <https://www.thejavaprogrammer.com/get-current-date-time-java-8/>
- <https://dzone.com/articles/java-8-date-time-api-part-2>
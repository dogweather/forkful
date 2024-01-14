---
title:                "Java: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

가장 중요한 이유는 현재 날짜를 받아오는 것은 프로그래밍에서 자주 사용되는 기능 중 하나이기 때문입니다. 다양한 이유로 현재 날짜를 알아야 할 때가 있습니다. 예를 들어, 사용자의 계정을 만들 때나 파일의 수정 일자를 체크할 때 등 순간적인 날짜 정보가 필요할 수 있습니다.

## 어떻게

다음은 자바 코드를 사용하여 현재 날짜를 받아오는 여러 가지 방법입니다.

```Java
// 현재 날짜와 시간을 받아오는 기본 방법
LocalDateTime now = LocalDateTime.now();

// 특정 타임존의 현재 날짜와 시간을 받아오는 방법
ZoneId zoneId = ZoneId.of("Asia/Seoul");
ZonedDateTime nowInSeoul = ZonedDateTime.now(zoneId);

// 현재 날짜만 받아오는 방법
LocalDate today = LocalDate.now();

// 현재 시간만 받아오는 방법
LocalTime currentTime = LocalTime.now();

// 현재 날짜와 시간 정보를 다루는 방법
System.out.println("현재 시간: " + now);
System.out.println("서울 시간: " + nowInSeoul);
System.out.println("오늘 날짜: " + today);
System.out.println("현재 시간: " + currentTime);
```

위의 코드를 실행하면 다음과 같은 출력을 볼 수 있습니다.

```
현재 시간: 2021-09-21T11:30:45.234  // 포맷은 ISO-8601를 따름
서울 시간: 2021-09-21T11:30:45.234+09:00[Asia/Seoul]
오늘 날짜: 2021-09-21
현재 시간: 11:30:45.234
```

이처럼 자바에서는 `now()` 메소드를 사용하여 현재 날짜와 시간을 받아오는 것이 가능합니다. 또한 타임존을 지정하여 특정 지역의 현재 날짜와 시간을 받아올 수도 있습니다.

## 깊이 살펴보기

코드를 실행한 결과를 확인하면 현재 날짜와 시간 정보는 포맷이 정해져 있지 않다는 것을 알 수 있습니다. 이는 자바에서 내부적으로 `java.time.format.DateTimeFormatter` 클래스를 사용하여 날짜와 시간 정보를 포맷하는 것으로 알고 있습니다.

하지만 이 클래스를 직접 사용하여 포맷을 지정하고 현재 날짜와 시간을 출력해보면 결과가 똑같이 나오는 것을 볼 수 있습니다.

```Java
// 현재 날짜를 BI/MI/YYYY 포맷으로 포맷팅하는 방법
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("MM/dd/yyyy");
String formattedDate = now.format(formatter);

// 현재 시간을 HH:mm 포맷으로 포맷팅하는 방법
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm");
String formattedTime = now.format(formatter);

// 포맷팅한 날짜와 시간 정보를 출력
System.out.println("포맷팅한 날짜 정보: " + formattedDate);
System.out.println("포맷팅한 시간 정보: " + formattedTime);
```

위의 코드를 실행하면 다음과 같은 출력을 볼 수 있습니다.

```
포맷팅한 날짜 정보: 09/21/2021
포맷팅한 시간 정보: 11:30
```

즉, 자바에서는 포맷 지정이 가능하도록 다양한 메소드를 제공해주고 있기 때문에 필요에 따라 날짜와 시간 정보를 다양한 포맷으로 변환할
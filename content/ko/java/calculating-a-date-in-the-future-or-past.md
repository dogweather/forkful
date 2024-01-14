---
title:    "Java: 미래나 과거의 날짜 계산하기"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜
날짜를 계산하는 것에 대해 알아보는 이유는 다양합니다. 예를 들어, 생일이나 결혼식과 같은 특별한 이벤트를 계획할 때 비롯하여 특정 날짜까지 몇 일이 남았는지 확인하고 싶을 때 등의 상황에서 반드시 필요한 기능입니다.

## 어떻게
자바에서 날짜를 계산하는 방법은 매우 간단합니다. 먼저 ```LocalDate``` 클래스를 사용하여 오늘의 날짜를 가져온 다음, ```.plusDays()``` 메소드를 사용하여 지정한 날 수 만큼 날짜를 더하거나 ```.minusDays()``` 메소드를 사용하여 지정한 날 수 만큼 날짜를 뺄 수 있습니다. 아래는 예시 코드와 출력 결과입니다.

```Java
LocalDate today = LocalDate.now();
LocalDate futureDate = today.plusDays(10);
LocalDate pastDate = today.minusDays(5);

System.out.println("오늘 날짜: " + today);
System.out.println("10일 뒤의 날짜: " + futureDate);
System.out.println("5일 전의 날짜: " + pastDate);
```

출력 결과:

```
오늘 날짜: 2021-07-23
10일 뒤의 날짜: 2021-08-02
5일 전의 날짜: 2021-07-18
```

## 깊이 파고들기
더 많은 기능을 원한다면, ```LocalDate``` 클래스뿐만 아니라 ```LocalDateTime```, ```ZonedDateTime``` 등 다양한 날짜 및 시간 관련 클래스를 사용할 수 있습니다. 또한, 날짜 간의 차이를 계산하고 비교하는 메소드도 있으므로 유용하게 활용할 수 있습니다. 자세한 내용은 공식 문서를 참고하시고 실제로 코드를 작성하며 익혀보시길 추천 드립니다.

## 참고
- [Java 8 날짜 및 시간 API 문서](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [날짜와 시간을 다루는 방법 - Baeldung 블로그](https://www.baeldung.com/java-date-time)
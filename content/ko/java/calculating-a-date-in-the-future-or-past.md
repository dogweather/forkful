---
title:    "Java: 미래나 과거의 날짜 계산하기"
keywords: ["Java"]
---

{{< edit_this_page >}}

## 왜 
날짜를 미래나 과거로 계산하는 것에 관심이 있는 사람이라면 여기서 흥미로운 정보를 얻을 수 있을 것입니다!

## 어떻게
이제부터는 자바를 사용하여 간단하게 날짜를 계산하는 방법에 대해 알아보겠습니다.

```Java
// 현재 날짜를 기준으로 1년 후의 날짜 계산하기
LocalDate currentDate = LocalDate.now();
LocalDate futureDate = currentDate.plusYears(1);
System.out.println("1년 후 날짜: " + futureDate);
// Output: 1년 후 날짜: 2022-07-12

// 현재 날짜를 기준으로 2달 전의 날짜 계산하기
LocalDate currentDate = LocalDate.now();
LocalDate pastDate = currentDate.minusMonths(2);
System.out.println("2달 전 날짜: " + pastDate);
// Output: 2달 전 날짜: 2021-05-12

// 특정 날짜를 기준으로 1주일 후의 날짜 계산하기
LocalDate specificDate = LocalDate.of(2021, 10, 1);
LocalDate futureDate = specificDate.plusWeeks(1);
System.out.println("1주일 후 날짜: " + futureDate);
// Output: 1주일 후 날짜: 2021-10-08
```

## 심층 분석
사실 자바에서 날짜를 계산하는 방법은 간단합니다. LocalDate 클래스의 `plus()` 혹은 `minus()` 메소드를 사용하여 미래나 과거의 날짜를 계산할 수 있습니다. 이 메소드는 인자로 숫자와 TemporalUnit을 받습니다. 숫자는 더하거나 뺄 날짜의 크기를 나타내며, TemporalUnit은 날짜의 단위를 나타냅니다. 예를 들어 `plusYears(1)`라면 1년 후의 날짜를 계산하게 되는 것이고, `minusDays(7)`이라면 1주일 전의 날짜를 계산하게 됩니다.

## 더 자세한 정보
만약 자바에서 날짜를 계산하는 방법에 대해 더 깊이 알아보고 싶다면 [Oracle Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)을 참고해보세요.

## 관련 링크
- [Java로 날짜와 시간 다루기](https://www.itworld.co.kr/news/161949)
- [Java 날짜 계산 예제와 방법](https://myhappyman.tistory.com/22)
- [Java 날짜 관련 유용한 메소드 모음](https://velog.io/@skyepodium/Java-%EB%82%A0%EC%A7%9C-%EA%B4%80%EB%A0%A8-%EC%9C%A0%EC%9A%A9%ED%95%9C-%EB%A9%94%EC%86%8C%EB%93%9C-%EB%AA%A8%EC%9D%8C)
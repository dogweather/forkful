---
title:                "Java: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 왜?

자바 프로그래밍을 배우고 사용하는 프로그래머라면, 두 날짜를 비교하는 것은 매우 중요한 기능입니다. 날짜를 비교하여 이전 날짜인지, 이후 날짜인지 아니면 같은 날짜인지 확인하는 것은 많은 작업에서 필수적입니다. 따라서 두 날짜를 비교하는 방법을 알아보겠습니다.

## 어떻게?

먼저, 두 날짜를 비교하기 전에 두 날짜를 `Date` 객체로 변환해야 합니다. 그리고 `compareTo()` 메소드를 사용하여 두 날짜를 비교할 수 있습니다. 이 메소드는 첫 번째 날짜가 두 번째 날짜보다 이전인 경우 음수를, 같은 경우 0을, 이후인 경우 양수를 반환합니다. 이를 이용해 다음과 같이 비교할 수 있습니다.

```Java 
Date firstDate = new Date(2019, 8, 13); // 첫 번째 날짜 생성
Date secondDate = new Date(2020, 8, 13); // 두 번째 날짜 생성

// 첫 번째 날짜가 두 번째 날짜보다 이전인지 비교
if(firstDate.compareTo(secondDate) < 0){
    System.out.println("첫 번째 날짜가 두 번째 날짜보다 이전입니다.");
} else if(firstDate.compareTo(secondDate) == 0) { // 같은 경우
    System.out.println("두 날짜는 같습니다.");
} else { // 이후인 경우
    System.out.println("두 번째 날짜가 첫 번째 날짜보다 이전입니다.");
}
```

출력결과:

```
첫 번째 날짜가 두 번째 날짜보다 이전입니다.
```

또 다른 방법으로는, `getTime()` 메소드를 사용하여 날짜의 밀리초 값으로 비교하는 방법도 있습니다. 이를 이용하면 아래와 같이 코드를 작성할 수 있습니다.

```Java
Date firstDate = new Date(2019, 8, 13); // 첫 번째 날짜 생성
Date secondDate = new Date(2020, 8, 13); // 두 번째 날짜 생성

// 첫 번째 날짜의 밀리초 값이 두 번째 날짜의 밀리초 값보다 작은지 비교
if(firstDate.getTime() < secondDate.getTime()){
    System.out.println("첫 번째 날짜가 두 번째 날짜보다 이전입니다.");
} else if(firstDate.getTime() == secondDate.getTime()) { // 같은 경우
    System.out.println("두 날짜는 같습니다.");
} else { // 이후인 경우
    System.out.println("두 번째 날짜가 첫 번째 날짜보다 이전입니다.");
}
```

출력결과:

```
첫 번째 날짜가 두 번째 날짜보다 이전입니다.
```

## 깊게 들어가기

자바에서 `Date` 클래스는 `java.util` 패키지에 포함되어 있으며, 날짜와 시간을 다루는 기능들을 제공합니다. `compareTo()`와 `getTime()` 메소드 외에도 `equals()` 메소드를 사용하여 두 날짜가 같은지 비교할 수 있고, `after()`와 `before()` 메소드를 사용하여 각각 두 날짜가 이후인지, 이전인지 비교할 수도 있습니다. 또한, `Calendar` 클래스를 사용하여 더 다양한 방법으로 날짜와 시간을 다룰 수 있습니다.

# 관련 정보

- [
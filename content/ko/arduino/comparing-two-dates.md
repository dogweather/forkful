---
title:    "Arduino: 두 날짜 비교하기"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 왜 두 날짜를 비교할까요?

아두이노 프로그래밍을 하는 데에는 다양한 목적이 있을 수 있습니다. 그 중 하나는 날짜를 비교하는 것입니다. 날짜를 비교하면 다양한 프로젝트에 활용할 수 있습니다. 그러나 날짜를 비교하는 방법을 알지 못한다면 어려울 수 있습니다. 그래서 이번 포스트에서는 날짜 비교의 중요성과 그 방법에 대해 알아보고자 합니다.

## 어떻게 하면 될까요?

날짜를 비교하는 방법에 대해 알아보겠습니다. 먼저 두 날짜를 비교하려면 먼저 이 두 날짜를 변수로 선언해야 합니다. 그리고 변수들을 이용하여 비교하는 방법은 다양합니다. 예를 들어, 두 날짜가 같은지 체크하기 위해서는 ```==``` 연산자를 사용합니다. 다음 코드를 참고해보세요.

```Arduino
int firstDate = 20201208;
int secondDate = 20201208;

if (firstDate == secondDate) {
  Serial.println("두 날짜가 같습니다.");
}
```

위 코드에서는 두 날짜가 같기 때문에 "두 날짜가 같습니다."라는 메시지가 시리얼 모니터에 출력됩니다. 또한 두 날짜가 다른지 체크하기 위해서는 ```!=``` 연산자를 사용하면 됩니다. 다음 코드를 참고해보세요.

```Arduino
int firstDate = 20201208;
int secondDate = 20201209;

if (firstDate != secondDate) {
  Serial.println("두 날짜가 다릅니다.");
}
```

위 코드에서는 두 날짜가 다르기 때문에 "두 날짜가 다릅니다."라는 메시지가 시리얼 모니터에 출력됩니다. 이와 같이 두 날짜를 비교할 때는 변수를 이용하여 다양한 연산자를 사용할 수 있습니다.

## 더 깊이 들어가보겠습니다.

이제 더 깊이 들어가서 날짜 비교에 대해 알아보겠습니다. 날짜 비교를 할 때는 알아야할 중요한 개념이 있습니다. 바로 Unix 시간입니다. Unix 시간은 1970년 1월 1일부터 경과한 초를 의미합니다. 이를 통해 두 날짜를 비교할 수 있습니다. 예를 들어, 2020년 12월 8일은 1607376000초, 2020년 12월 9일은 1607462400초로 표현할 수 있습니다. 이를 바탕으로 두 날짜를 빼면 날짜 간의 차이를 알 수 있습니다. 또한 다음 링크는 시리얼 모니터에서 불러온 날짜를 변환하는 방법에 대한 정보를 제공합니다.

출처: https://arduino.stackexchange.com/questions/18929/how-do-i-do-weekday-and-date-time-functions-for-a-news-ticker

## 중요 링크

- https://www.arduino.cc/
- https://www.arduino.cc/reference/ko/
- https://www.arduino.cc/en/Tutorial/BuiltInExamples/TimeSerial
- https://www.arduino.cc/reference/en/language/functions/time/millis/
- https://www.arduino.cc/reference/en/language/functions/time/micros/
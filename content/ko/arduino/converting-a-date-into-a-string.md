---
title:    "Arduino: 날짜를 문자열로 변환하기"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# 왜 날짜를 문자열로 변환해야 하는가?

날짜를 문자열로 변환하는 과정은 프로그래밍에서 자주 사용되는 작업 중 하나입니다. 특히, 센서 데이터를 수집하고 저장하거나 날짜와 시간을 로깅하는 등의 작업에서 필수적입니다. 이러한 과정을 굉장히 효율적으로 처리하기 위해서는 날짜를 문자열로 변환하는 방법을 배우는 것이 중요합니다.

## 어떻게 날짜를 문자열로 변환할 수 있을까?

Arduino에서는 기본적으로 `String` 타입을 제공하며, 이를 활용하여 날짜를 문자열로 변환할 수 있습니다. 우선, `String` 타입의 변수를 선언하고 `print()` 함수를 사용하여 날짜(년, 월, 일)와 시간(시, 분, 초) 값을 출력합니다. 다음은 현재 날짜와 시간을 문자열로 변환하는 예시 코드입니다.

```Arduino
String date = "";

int year = 2021;
int month = 9;
int day = 1;

int hour = 13;
int minute = 30;
int second = 0;

// 날짜를 문자열로 변환
date += String(year) + "/" + String(month) + "/" + String(day);
date += " " + String(hour) + ":" + String(minute) + ":" + String(second);
// 출력
Serial.println(date);
```

위의 코드를 실행하면 다음과 같은 결과를 얻을 수 있습니다.

```
2021/9/1 13:30:00
```

## 깊게 들어가보기

날짜를 문자열로 변환할 때 주의해야 할 점은 `String` 타입의 변수가 메모리를 많이 차지한다는 것입니다. 그렇기 때문에 날짜를 매번 문자열로 변환하는 것은 메모리 낭비를 초래할 수 있습니다. 따라서, 여러 가지 날짜 형식을 만들고자 할 때는 메모리를 효율적으로 활용할 수 있는 방법을 고려해야 합니다.

또한, 날짜를 문자열로 변환할 때에는 각각의 자리수에 맞게 `String`을 조합해야 합니다. 예를 들어, 한 자리수의 경우 앞에 0을 채워주어야 하므로 `String(month)`를 사용하는 것이 아니라 `String("0" + String(month))`과 같이 사용해야 합니다.

이와 같은 세부적인 사항들을 고려하면 날짜를 문자열로 변환하는 과정을 더욱 능률적으로 처리할 수 있습니다.

## 관련 자료

- [Arduino Reference - String](https://www.arduino.cc/reference/ko/language/variables/data-types/string/)
- [날짜와 시간을 문자열로 변환하는 방법](https://cactusphere.co.kr/archives/6777)
- [시간과 날짜 관련 함수](https://www.arduino.cc/reference/ko/language/functions/time/)
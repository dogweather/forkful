---
title:                "날짜 두 개 비교하기"
html_title:           "Arduino: 날짜 두 개 비교하기"
simple_title:         "날짜 두 개 비교하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇인가?& 왜 그래? :
두 개의 날짜를 비교하는 것은 프로그래머들이 날짜를 비교할 때 사용하는 방법입니다. 이는 특정 날짜가 다른 날짜보다 뒤에 있는지 앞에 있는지 알아내는 데 사용됩니다.

## 하는 법:
아래는 두 개의 날짜를 비교하는 간단한 샘플 코드입니다. 코드 블록 안에 있는 **Arduino ..** 부분은 실제 Arduino 코드를 작성하는 부분입니다.

```Arduino
int date1 = 20200622; // 첫 번째 날짜
int date2 = 20200615; // 두 번째 날짜

// 첫 번째 날짜가 더 큰지 비교
if (date1 > date2) {
  Serial.println("첫 번째 날짜가 두 번째 날짜보다 뒤에 있습니다.");
} 

// 두 번째 날짜가 더 큰지 비교
if (date2 > date1) {
  Serial.println("두 번째 날짜가 첫 번째 날짜보다 뒤에 있습니다.");
} 

// 두 날짜가 같은지 확인
if (date1 == date2) {
  Serial.println("두 날짜가 같습니다.");
}
```

아래는 상기 코드의 출력 예시입니다. Serial Monitor를 열어서 코드를 실행시켜보세요!

```
첫 번째 날짜가 두 번째 날짜보다 뒤에 있습니다.
```

## 깊이 다뤄보기:
### 역사적 배경:
날짜를 비교하는 방법은 과거부터 프로그래머들이 많이 사용해온 기술 중 하나입니다. 이를테면 예약 시스템이나 이벤트 관리 시스템에서 자주 사용됩니다.

### 대안:
날짜 비교를 위해 다양한 알고리즘이나 라이브러리가 존재합니다. 이 중 가장 대표적인 것은 시간 차이를 계산하는 기능을 갖춘 시간 관리 라이브러리입니다. 이를 이용해 특정 날짜와 현재 시간의 차이를 계산할 수 있습니다.

### 구현 세부사항:
날짜를 비교할 때는 주로 년, 월, 일을 비교합니다. 이를 비교하기 위해 각각의 날짜를 정수로 변환한 뒤 크기를 비교하는 방법을 사용합니다. 이때 주의해야 할 점은 날짜를 정확하게 입력하는 것이 중요합니다.

## 참고 자료:
- [Arduino에서 날짜 비교하기](https://circuits4you.com/2016/11/15/arduino-date-time/)
- [시간 관리 라이브러리](https://playground.arduino.cc/Code/Time/)
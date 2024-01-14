---
title:                "C++: 미래나 과거의 날짜 계산하기"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜
날짜를 미래나 과거로 계산하는 것이 왜 중요한지에 대해 소개하겠습니다.

우리는 모두 일상 생활에서 날짜를 계산하는 일이 많이 있습니다. 미래에 예정된 일정을 확인하거나 과거에 일어난 사건을 기억하기 위해 날짜를 계산하는 경우가 대표적입니다. 따라서 프로그래밍에서도 날짜를 계산하는 기능은 매우 유용합니다.

## 어떻게
C++에서 날짜를 계산하는 방법을 예시 코드와 함께 알려드리겠습니다. 아래의 코드 블록은 미래 날짜를 계산하는 함수를 정의하는 코드입니다.

```C++
#include <iostream>

using namespace std;

// 미래 날짜를 계산하는 함수
string futureDate(int days) {
    string date = "2021/10/10"; // 현재 날짜
    int futureDays = days + 10; // 현재 날짜에서 10일 미래의 날짜 계산
    date[date.size() - 2] = futureDays / 10 + '0'; // 날짜의 십의 자리 변환
    date[date.size() - 1] = futureDays % 10 + '0'; // 날짜의 일의 자리 변환
    return date;
}

int main() {
    cout << "나는 10일 후가 " << futureDate(10) << "라는 걸 안다!" << endl;
    return 0;
}
```

위의 코드를 실행하면 "나는 10일 후가 2021/10/20라는 걸 안다!"라는 출력 결과를 얻을 수 있습니다. 이처럼 날짜를 계산하는 함수를 만들어서 미래 날짜를 쉽게 계산할 수 있습니다.

## 깊이 파고들기
미래 날짜를 계산하는 것 외에도, 과거 날짜를 계산하는 함수를 만들 수 있습니다. 또한, 요일이나 월을 기준으로 날짜를 계산하는 방법도 있습니다. 이러한 기능을 적절하게 활용하면 더욱 다양한 날짜 계산을 수행할 수 있습니다.

## 관련 정보
날짜 계산에 대한 더 많은 정보를 얻고 싶다면 아래의 링크들을 참고해보세요.

- [C++ 함수 만드는 방법](https://modoocode.com/116)
- [C++에서 문자열 다루기](https://doeofpost.tistory.com/34)
- [C++에서 날짜 처리하는 방법](https://doeofpost.tistory.com/36)
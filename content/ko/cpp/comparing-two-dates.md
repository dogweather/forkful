---
title:                "C++: 두 날짜 비교하기"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜
두 날짜를 비교하는 것에 관심을 가져야 할까요? 날짜는 컴퓨터 과학에서 매우 중요한 역할을 합니다. 예를 들어, 날짜를 기준으로 일정한 기간을 지정하기 위해 날짜를 사용합니다. 따라서 날짜를 비교하는 것은 매우 유용한 프로그래밍 기술입니다.

## 방법
아래에는 C ++로 두 날짜를 비교하는 방법과 샘플 출력이 포함 된 코딩 예제가 있습니다.

```C++
#include <iostream>
using namespace std;

int main(){
    // 날짜를 변수로 저장합니다.
    int date1 = 20211112;
    int date2 = 20200601;

    // 두 날짜를 비교합니다.
    if(date1 > date2){
        cout << "두 번째 날짜가 첫 번째 날짜보다 이전입니다.";
    } else if(date1 < date2){
        cout << "첫 번째 날짜가 두 번째 날짜보다 이전입니다.";
    } else {
        cout << "두 날짜가 같습니다.";
    }

    return 0;
}
```

위 코드의 출력은 다음과 같습니다:

```
두 번째 날짜가 첫 번째 날짜보다 이전입니다.
```

위 예제에서는 날짜를 정수로 비교했지만, 실제로는 일반적으로 년, 월, 일과 같은 구성 요소를 사용하여 더 정확하게 비교하게 됩니다. 또한 프로그래머는 자신의 필요에 따라 다른 비교 연산자를 사용할 수 있습니다.

## 깊이 파고들기
날짜를 비교하는 방법은 프로그래밍에서 매우 유용하며, 여러 가지 방법으로 구현할 수 있습니다. 예를 들어, 두 날짜를 문자열로 변환하여 비교할 수도 있고, 년, 월, 일을 각각 변수로 사용하여 비교할 수도 있습니다. 따라서 프로그래밍에서 날짜를 비교하는 데는 정확한 방법이나 규칙이 없으며, 프로그래머가 자신의 필요에 따라 적절한 방법을 선택할 수 있습니다.

## 관련 자료
- [C++에서 두 날짜 비교하기](https://www.programiz.com/cpp-programming/datetime-comparison)
- [C++에서 시간과 날짜 다루기](https://www.boost.org/doc/libs/1_64_0/doc/html/date_time.html)
- [날짜와 시간을 처리하는 C++ 라이브러리](https://github.com/HowardHinnant/date)
- [C++에서 날짜 비교하기 - 연산자 오버로딩 예제](https://stackoverflow.com/questions/16293896/comparing-dates-in-c-with-operators)
- [ISO 8601: 날짜와 시간 표현 표준](https://en.wikipedia.org/wiki/ISO_8601)
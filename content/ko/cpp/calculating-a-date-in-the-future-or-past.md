---
title:    "C++: 미래나 과거의 날짜 계산하기"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 프로그래밍을 해야 하는 이유는 무엇일까요? 간단히 말해서, 우리는 특정한 날짜를 계산하여 일정한 시간만큼 이동하거나 얼마나 지난 날짜인지 알 수 있기 때문입니다.

## 계산 방법

날짜를 계산하는 방법은 매우 간단합니다. 먼저, 계산하고자 하는 과거나 미래의 날짜를 알아야 합니다. 그리고 그 날짜를 기준으로 얼마만큼 이동하고 싶은지, 혹은 어느 방향으로 이동하고 싶은지를 정해야 합니다. 이 정보를 바탕으로 ```C++``` 프로그래밍 언어를 이용하여 다음의 예제 코드를 사용할 수 있습니다.

```C++
#include <iostream>
#include <iomanip>
#include <chrono>

using namespace std;

// 오늘 날짜를 기준으로 1달 후의 날짜를 계산하는 예제 코드
int main() {
  // 오늘의 날짜를 저장하는 변수
  auto today = chrono::system_clock::now();
  // 월 단위로 계산하기 위해 1달을 추가
  auto future_date = today + chrono::months(1);
  // 날짜를 출력하기 위해 필요한 포맷
  auto formatDate = chrono::system_clock::to_time_t(future_date);
  // 포맷에 맞는 날짜 출력
  cout << put_time(localtime(&formatDate), "%Y/%m/%d") << endl;
  return 0;
}
```

위 예제 코드를 실행하면 현재 날짜를 기준으로 1달 뒤의 날짜가 출력됩니다. 이와 같은 방법으로 다양한 시간 단위의 계산을 할 수 있으며, 필요에 따라서 날짜를 빼거나 더하여 원하는 결과를 얻을 수 있습니다.

## 깊게 파고들어보기

날짜 계산은 우리 일상 생활에서 매우 중요한 역할을 합니다. 예를 들어, 거래를 위해 공휴일을 고려하여 특정 날짜를 계산하거나, 여행 계획을 세울 때 날짜를 계산하고 예약하는 등 다양한 분야에서 사용될 수 있습니다. 이러한 날짜 계산을 컴퓨터 프로그램을 통해 더 쉽고 정확하게 할 수 있습니다. 날짜를 계산하는 방법은 프로그래밍 언어마다 다소 차이가 있지만 참고할만한 예제 코드나 라이브러리가 많이 존재합니다. 또한, 날짜 계산 과정에서 고려해야 할 점들도 많기 때문에 깊게 파고들어보면 더 많은 지식을 얻을 수 있습니다.

## 또 다른 정보

* [날짜 계산을 위한 C++ 라이브러리](https://www.boost.org/doc/libs/1_68_0/doc/html/date_time.html)
* [날짜 계산 예제 코드](https://gist.github.com/tianon/6845679)
* [나무위키: Excel에서 날짜 계산하는 방법](https://namu.wiki/w/Excel/%EB%82%A0%EC%A7%9C%20%EA%B3%84%EC%82%B0)
---
title:    "C++: 현재 날짜 가져오기"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜
현재 날짜를 가져오는 것을 왜 하게 될 지 궁금하신가요? 프로그래밍을 할 때 예약된 작업을 수행하거나 이벤트의 일자를 확인할 때 자주 사용됩니다.

## 어떻게
```C++
#include <iostream>
#include <ctime>

using namespace std;

int main()
{
  // 현재 시간 가져오기
  time_t now = time(0);
  
  // 시간 구조체로 변환
  tm *ltm = localtime(&now);
  
  // 날짜 출력
  cout << "현재 날짜: " << ltm->tm_mday << "/" << 1 + ltm->tm_mon << "/" << 1900 + ltm->tm_year << endl;
  
  return 0;
}

```
출력:
현재 날짜: 21/10/2021

## 심층 탐구
현재 날짜를 가져오기 위해서는 시스템의 시계를 사용해야 합니다. 시스템의 시계는 초 단위로 나누어진 시간 값을 저장하는 변수입니다. 이 값은 1970년 1월 1일 이후 경과한 초의 수를 나타냅니다. 따라서 이 값을 구한다면 현재 시간을 알 수 있습니다. 이번 예제에서는 C++의 <ctime> 라이브러리를 사용했지만, 다른 라이브러리들을 이용해도 동일한 결과를 얻을 수 있습니다.

## 참고문헌
- [C++ <ctime> 라이브러리](https://www.cplusplus.com/reference/ctime/)
- [시스템 시계의 역할](https://www.geeksforgeeks.org/time-function-in-c/)
- [C++에서 날짜 형태로 출력하기](https://www.includehelp.com/cpp-programs/print-current-date-and-time-in-cpp.aspx)

## 참고
*이 블로그 글은 개인적으로 학습한 내용을 정리하여 작성된 글입니다. 언제나 꾸준한 학습의 중요성을 강조합니다.*
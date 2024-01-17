---
title:                "두 날짜 비교하기"
html_title:           "C++: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
날짜 비교는 날짜 간의 차이를 계산하여 두 날짜를 비교하는 것입니다. 프로그래머들은 날짜를 비교하는 이유는 날짜와 시간을 다루는 작업에서 정확성이 중요하기 때문입니다.

## 방법:
C++의 내장 함수인 ```difftime```를 사용하여 날짜간의 차이를 계산할 수 있습니다. 다음은 날짜 비교를 하기 위한 간단한 예제 코드입니다:

```
#include <iostream>
#include <ctime>

int main() {
  struct tm date1 = {0,0,0,3,1,119,0,0,0};
  //2020년 3월 1일
  struct tm date2 = {0,0,0,3,1,119,0,0,0};
  //2020년 3월 1일
  double time_diff = difftime(mktime(&date1), mktime(&date2));
  
  std::cout << "날짜 간의 차이는 " << time_diff << "초 입니다." << std::endl;
  return 0;
}
```

위 코드를 실행하면 "날짜 간의 차이는 0초 입니다."라는 결과가 나올 것입니다.

## 깊이 파고들기:
날짜를 비교하는 함수는 컴퓨터 운영체제에서 재는 기준 시간인 에포크(epoch)를 기반으로 작동합니다. 에포크는 보통 1970년 1월 1일 자정 UTC를 가리킵니다. 이 기준 시간을 이용해 알고리즘적으로 날짜 간의 차이를 계산하고 결과를 초 단위로 반환합니다. C++ 이외에도 다른 언어들에서도 비슷한 기능을 제공합니다.

## 관련자료:
[C++ Reference - difftime](http://www.cplusplus.com/reference/ctime/difftime/)
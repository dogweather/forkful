---
title:                "C++: 현재 날짜 받기"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 왜 현재 날짜를 알아야 할까요?

현재 날짜를 알아야 하는 이유는 다양합니다. 가장 일반적인 이유는 프로그래밍에서 날짜와 시간을 사용해야 할 때입니다. 예를 들어, 특정 기간 내에 작업이 실행되도록 하거나, 특정 날짜에 특정 이벤트를 발생시키도록 프로그램을 작성해야 할 때가 있습니다. 또는 파일에 작성된 날짜를 자동으로 업데이트하거나, 사용자에게 현재 날짜를 노출하거나, 신용카드나 여권 등에 날짜 정보를 첨부해야 할 때도 있습니다.

# 어떻게 현재 날짜를 가져올 수 있나요?

C++에서 현재 날짜를 가져오는 방법은 다양하지만, 가장 간단하게는 <ctime> 헤더 파일을 포함하고 "std::time" 함수를 사용하는 것입니다. 이 함수는 시간 정보를 저장하는 "time_t" 변수를 반환합니다. 그러나 우리는 이것을 읽고 해석하기에 편하도록 "std::tm" 구조체로 변환해야 합니다. 다음은 "std::time"을 사용하여 현재 날짜를 가져오고 그것을 "std::tm"으로 변환하는 예제 코드입니다.

```C++
#include <iostream>
#include <ctime>

int main() {
    std::time_t now = std::time(0);
    std::tm* current_time = std::localtime(&now);

    std::cout << "현재 날짜: " << current_time->tm_mon + 1 << "/"
         << current_time->tm_mday << "/" << current_time->tm_year + 1900 << std::endl;
    return 0;
}
```

위 코드를 실행하면 현재 날짜가 출력됩니다. "std::localtime" 함수는 "time_t" 변수를 "std::tm" 구조체로 변환하고, "tm_mon"은 월을 나타내는데 0부터 시작하기 때문에 1을 더해준 후 출력합니다. "tm_mday"는 일을, "tm_year"는 연도를 의미하는데, 현재 날짜를 얻기 위해 현재 시스템의 시간대에 맞는 날짜를 반환합니다.

# 깊게 들어가보자

이전 예제는 단순히 현재 날짜를 출력하는 것에 그치지만 실제로는 더 많은 작업이 필요합니다. 우선, 우리는 날짜를 출력할 때 월과 일이 한 자리 수인 경우 두 자리로 표현해주는 것이 좋습니다. 따라서 위 코드에서도 "%02d"와 같은 형식 지정자를 사용해주는 것이 좋습니다. 또한, "std::tm" 구조체가 시간 정보를 저장하는 데 사용하는 멤버 변수들은 C의 표준 라이브러리인 "struct tm"에서 가져오는데, 이 구조체의 멤버 변수들은 모두 정수 값이기 때문에 원하는 문자열 형태로 변경하려면 많은 추가 작업이 필요합니다. 이를 더 쉽게 해주는 것이 C++의 "std::put_time" 함수입니다. 다음은 위 코드를 "std::put_time"을 사용하여 더 간편하게 출력하는 예제입니다.

```C++
std::cout << "현재 날짜: " << std::put_time(current_time, "%m/%d/%Y") << std::endl;
```

# 더 많은 정보가 필요하다면

현재 날짜를 가져오는 방법은
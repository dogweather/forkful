---
title:    "C++: 현재 날짜 가져오기"
keywords: ["C++"]
---

{{< edit_this_page >}}

## 왜 당신은 오늘의 날짜를 가져오는 것에 대해 관심이 있을까요?

우선, 우리는 모두 현재 날짜를 알고 싶어합니다. 프로그래밍의 경우, 특정 작업을 수행하려면 현재 날짜를 알아야 할 수 있습니다. 또한 미래에 실행되는 코드를 작성한다면 이전에 작성된 코드와의 비교를 위해 현재 날짜를 저장할 수 있습니다. 어떠한 이유에서든, 현재 날짜를 가져오는 프로그래밍 방법은 매우 유용합니다.

## 어떻게 현재 날짜를 가져올 수 있을까요?

자, 우리가 할 일은 간단합니다. C++에서 현재 날짜를 가져오는 가장 일반적인 방법은 `ctime` 라이브러리의 `std::time` 함수를 사용하는 것입니다. 다음은 `std::time` 함수를 사용하여 현재 날짜를 가져오는 간단한 예제 코드입니다.

```C++
#include <iostream>
#include <ctime>

int main() {
    std::time_t now = std::time(NULL);
    std::tm *ptm = std::localtime(&now);
    std::cout << "현재 날짜: " << 1 + ptm->tm_mon << "/" << ptm->tm_mday << "/" << 1900+ptm->tm_year;
}
```

이 코드의 주요 부분은 `std::time` 함수로 현재 시간을 불러오는 것입니다. 이를 `std::localtime` 함수를 사용하여 더 쉽게 이해할 수 있는 형식으로 변환한 후, 원하는 형식으로 출력하면 됩니다. 이 예제에서는 월/일/년도 형식으로 출력했습니다.

## 딥 다이브

여기서 우리는 조금 더 깊이있게 현재 날짜를 가져오는 방법을 살펴볼 것입니다. `std::time` 함수를 사용하여 얻은 날짜는 운영체제의 시간 설정에 따라 다를 수 있습니다. 따라서, 유닉스 시간 (1970년 1월 1일부터 현재까지의 초 값을 나타내는 값)을 사용하여 프로그래밍하는 것이 더 안전한 방법입니다. `std::time` 함수가 반환하는 값은 유닉스 시간이므로 이를 사용하여 프로그래밍하면 운영체제에 상관없이 동일한 결과를 얻을 수 있습니다.

```C++
#include <iostream>
#include <ctime>

int main() {
    std::time_t now = std::time(NULL);
    std::cout << "현재 날짜: " << now;
}
```

또한, `std::time` 함수는 시스템 시간을 가져오는 것이기 때문에 사용자가 조작할 수 없습니다. 만약 사용자가 직접 날짜를 선택하고 싶다면, `std::chrono` 라이브러리를 사용하여 직접 시스템 시간을 설정하는 것이 가능합니다.

## 더 알아보기

이 글에서 우리는 C++로 현재 날짜를 가져오는 방법에 대해 알아보았습니다. 더 많은 자세한 정보를 알고 싶으시다면, 다음 링크들을 확인해보세요.

[ctime 라이브러리 참조](https://ko.wikipedia.org/wiki/Ctime)

[std::time 함수에 대한 Cplusplus.com 참조](https://www.cplusplus.com/reference/ctime/time/)

[std::chrono 라이브러리 참조](https://ko.cppreference.com/w/cpp/chrono)
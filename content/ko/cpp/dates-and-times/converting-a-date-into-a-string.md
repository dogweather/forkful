---
date: 2024-01-20 17:36:03.714098-07:00
description: "How to: (\uBC29\uBC95:) Sample Output."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.313163-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95:) Sample Output."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 28
---

## How to: (방법:)
```cpp
#include <iostream>
#include <iomanip>
#include <sstream>
#include <chrono>

int main() {
    // 현재 시간을 구함
    auto now = std::chrono::system_clock::now();
    std::time_t t = std::chrono::system_clock::to_time_t(now);
  
    // tm 구조체로 변환
    std::tm tm = *std::localtime(&t);
  
    // stringstream을 사용하여 날짜를 문자열로 포매팅
    std::stringstream ss;
    ss << std::put_time(&tm, "%Y-%m-%d");
  
    // 문자열로 결과 출력
    std::string date_str = ss.str();
    std::cout << "Formatted date: " << date_str << std::endl;
  
    return 0;
}
```

Sample Output:
```
Formatted date: 2023-01-30
```

## Deep Dive (심층 분석):
날짜를 문자열로 변환하는 것은 C++에서 다양한 방법으로 처리했습니다. C에서는 `strftime` 함수를 사용하였지만, C++11 이후부터는 `<chrono>` 라이브러리를 통해 더 강력하고 직관적인 방식이 도입되었습니다. 이전의 방식에 비해 `<chrono>`는 타임존 관리나 시간 연산을 더욱 쉽게 만들어 줍니다.

때로는 `boost::date_time` 라이브러리와 같은 대안도 고려됩니다. 하지만 표준 라이브러리의 강점은 대부분의 환경에서 바로 사용할 수 있다는 것입니다.

`std::put_time`는 C++11에 추가된 포매팅 함수로, `strftime` 스타일의 포맷 코드를 사용합니다. 구현 면에서는 `std::ostringstream` 대신 C 스타일 I/O를 사용할 수도 있지만, `std::ostringstream`은 타입 안정성과 재사용성 면에서 강점을 지닙니다.

## See Also (추가 자료):
- `<chrono>`. C++ Reference: https://en.cppreference.com/w/cpp/header/chrono
- `std::strftime`. C++ Reference: https://en.cppreference.com/w/cpp/chrono/c/strftime
- Boost Date_Time: https://www.boost.org/doc/libs/release/libs/date_time/
- `std::put_time`. C++ Reference: https://en.cppreference.com/w/cpp/io/manip/put_time

이러한 자료들을 통해 날짜와 시간에 대한 더욱 깊은 이해와 다양한 활용법을 배울 수 있습니다.

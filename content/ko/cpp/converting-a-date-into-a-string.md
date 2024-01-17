---
title:                "날짜를 문자열로 변환하기"
html_title:           "C++: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇 및 왜? 
날짜를 문자열로 변환한다는 것은 무엇인지에 대해 설명하는 2~3문장과 이를 하는 이유에 대해 알려준다. 프로그래머들이 이 작업을 하는 이유는 무엇인지도 설명한다. 

## 방법:
```C++
#include <iostream>
#include <string>
#include <ctime>

int main() {
	// 현재 날짜와 시간의 문자열 표현
	std::time_t now = std::time(nullptr);
	std::string date = std::ctime(&now);
	
	std::cout << "날짜와 시간: " << date << std::endl;
	
	// 지정된 날짜 및 시간의 문자열 표현
	std::tm date_time = { 0, 0, 8, 17, 4, 121 };
	std::time_t time = mktime(&date_time);
	std::string formatted_date = std::asctime(&date_time);
	
	std::cout << "일반 형식의 날짜와 시간: " << formatted_date << std::endl;

	return 0;
}
```
```text
날짜와 시간: Mon Jun 28 22:45:46 2021
일반 형식의 날짜와 시간: Mon Jun 29 00:45:08 2021
```

## 깊이 파고들기:
날짜를 문자열로 변환하는 작업은 프로그래밍에서 자주 사용되는 작업 중 하나이다. 예를 들어, 사용자가 입력한 날짜를 저장하거나 데이터베이스에서 날짜를 가져와 화면에 표시하는 등의 작업에서 날짜를 문자열로 변환하는 것이 필요하다. 이 작업은 C++의 표준 라이브러리 함수인 `strftime`을 사용하여 쉽게 구현할 수 있다. 또 다른 방법으로는, `strftime` 대신에 `stringstream`을 사용하여 문자열로 변환하는 것이 가능하다.

## 참고:
- http://www.cplusplus.com/reference/ctime/asctime/
- http://www.cplusplus.com/reference/ctime/mktime/
- http://www.cplusplus.com/reference/ctime/strftime/
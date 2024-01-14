---
title:                "C++: 날짜를 문자열로 변환하기"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜: 날짜를 문자열로 변환하는 것에 참여하는 이유는 무엇인가요?
 
날짜를 문자열로 변환하는 것은 프로그램에서 날짜를 더 쉽게 다룰 수 있도록 도와주기 때문입니다. 문자열 형식으로 저장된 날짜는 원하는 형식으로 출력하거나 비교하는 데 매우 유용합니다. 
 
## 어떻게: "```C++ ... ```" 코드 블록 내에 코딩 예제와 샘플 출력을 포함하여
 
```C++
#include <iostream>
#include <ctime>
#include <sstream>
using namespace std;

int main()
{
    // current date/time based on current system
    time_t now = time(0);
    
    // convert now to string form
    stringstream ss; 
    ss << now;
    string str_date = ss.str();
    
    // output the string date
    cout << str_date << endl;
    
    return 0;
}

/*
output:
1536076956
*/
```
 
이 코드 예제에서는 현재 시간을 불러와 `time_t` 데이터 유형으로 저장한 다음 이를 문자열로 변환하는 방법을 보여줍니다. `stringstream` 라이브러리를 사용하여 정수를 문자열로 변환할 수 있으며, 이를 활용하면 다양한 형식의 날짜를 문자열로 변환할 수 있습니다. `cout`을 사용하여 변환된 문자열을 출력할 수 있습니다. 
 
## 깊이 이해하기: 날짜를 문자열로 변환하는 더 깊은 정보
 
날짜를 문자열로 변환하는 데에는 여러 가지 방법이 있지만, 가장 간단하고 명확한 방법은 `time.h` 헤더 파일에서 제공하는 `time_t` 데이터 유형을 활용하는 것입니다. `stringstream` 라이브러리를 사용하여 정수를 문자열로 변환하는 방법은 다양한 형식의 데이터 유형을 문자열로 변환하는 데 유용하게 사용할 수 있습니다. 또한 `ctime` 라이브러리를 사용하면 다양한 형식의 시간, 날짜 정보를 얻을 수 있으며 이를 문자열로 변환할 수 있습니다. 
 
## 같이 보기 
 
- [C++ Reference - time_t](https://www.cplusplus.com/reference/ctime/time_t/)
- [C++ Reference - stringstream](https://www.cplusplus.com/reference/sstream/stringstream/)
- [C++ Reference - ctime](https://www.cplusplus.com/reference/ctime/)
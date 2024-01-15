---
title:                "두 날짜 비교"
html_title:           "C++: 두 날짜 비교"
simple_title:         "두 날짜 비교"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

두 날짜를 비교하는 방법을 배우는 것이 중요한 이유는 다양한 프로그래밍 작업에서 날짜를 다루는 것이 매우 일반적이기 때문입니다.

## 어떻게

C++에서 두 날짜를 비교하는 방법은 아래의 예시 코드와 함께 설명됩니다.

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main(){
    // 현재 날짜 가져오기
    time_t now = time(0);
    
    // tm 구조체에 현재 날짜 저장
    tm *currentDate = localtime(&now);
    
    // 비교할 날짜 설정
    // 년-월-일 순서로 입력해야 함
    string compareDate = "2021-05-20";
    
    // string 타입의 날짜를 tm 구조체로 변환
    tm convertedDate;
    strptime(compareDate.c_str(), "%Y-%m-%d", &convertedDate);
    
    // 두 날짜를 비교하여 출력
    if(currentDate->tm_year > convertedDate.tm_year){
        cout << "현재 날짜가 더 늦은 날짜입니다." << endl;
    }
    else if(currentDate->tm_year < convertedDate.tm_year){
        cout << "비교할 날짜가 더 늦은 날짜입니다." << endl;
    }
    else{
        if(currentDate->tm_mon > convertedDate.tm_mon){
            cout << "현재 날짜가 더 늦은 날짜입니다." << endl;
        }
        else if(currentDate->tm_mon < convertedDate.tm_mon){
            cout << "비교할 날짜가 더 늦은 날짜입니다." << endl;
        }
        else{
            if(currentDate->tm_mday > convertedDate.tm_mday){
                cout << "현재 날짜가 더 늦은 날짜입니다." << endl;
            }
            else if(currentDate->tm_mday < convertedDate.tm_mday){
                cout << "비교할 날짜가 더 늦은 날짜입니다." << endl;
            }
            else{
                cout << "같은 날짜입니다." << endl;
            }
        }
    }
    
    return 0;
}
```

위 코드를 컴파일하여 실행하면 현재 날짜와 비교할 날짜를 출력하여 두 날짜를 비교할 수 있습니다. 예시 코드에서는 현재 날짜는 시스템 시간을 기준으로 가져오고, 비교할 날짜는 string 타입으로 입력받아 tm 구조체로 변환합니다. 그 후, 두 날짜를 비교하여 결과를 출력합니다.

## 딥 다이브

비교할 날짜를 string 타입으로 입력받은 후 직접 tm 구조체로 변환하는 방법 외에도 라이브러리를 사용하여 간편하게 날짜를 비교하는 방법도 있습니다. 여러 가지 라이브러리 중 C++ 11부터 추가된 \<chrono\> 라이브러리는 날짜와 시간을 다루는 유용한 기능을 제공하여 더 쉽게 날짜를 비교할 수 있도록 도와줍니다.

## 참고 자료

- [C++ \<ctime\> 다루기](https://modoocode.com/215)
- [C++ 11 \<chrono\> 라이브러리 사용법](https://cplusplus.com/reference/chrono/)
- [C++ 날짜와 시간 다루기](https://www.learncpp.com/cpp-tutorial/513-c15-dates-and-times/)
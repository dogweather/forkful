---
title:    "C++: 날짜를 문자열로 변환하기"
keywords: ["C++"]
---

{{< edit_this_page >}}

# 왜
날짜를 문자열로 변환하는 것에 대해 이야기하려는 이유는 처리해야할 다양한 날짜 형식이 있기 때문입니다.

# 어떻게
```C++
#include <iostream> 
#include <stdio.h> 
#include <time.h> 
  
using namespace std; 
  
int main() 
{ 
    struct tm t; 
    t.tm_year = 2022 - 1900; // 년도 - 1900 
    t.tm_mon = 7; // 월 
    t.tm_mday = 29; // 일 
    char s[100]; 
    strftime(s, sizeof(s), "%Y%m%d", &t); 
    printf("Date is: %s", s); 
    return(0); 
} 
```
***출력: 20220729***

날짜를 문자열로 변환하는 방법은 다양합니다. 가장 일반적인 방법은 "strftime"을 사용하는 것입니다. 이 함수는 "시간 구조체"를 사용하여 형식을 지정하고 원하는 형식으로 출력할 수 있도록 해줍니다. 위의 예제는 YYYYMMDD 형식으로 날짜를 출력하는 간단한 코드입니다.

# 깊게 들어가기
시간 구조체와 함께 사용되는 다른 함수에는 "asctime()" 및 "ctime()"이 있습니다. 이들은 문자열로 포맷된 시간을 출력해주는데 사용됩니다. 그러나 문자열의 출력 형식은 지정할 수 없기 때문에 strftime보다는 덜 효율적인 방법입니다.

출력 형식에 대한 자세한 내용은 "strftime" 함수를 참조하고, 시간 정보를 구하는 다른 모든 함수들을 학습하는 것이 좋습니다. 이를 통해 다양한 날짜 및 시간 처리 기능을 구현할 수 있습니다.

# 참고
[출력 형식 지정하기](https://www.cplusplus.com/reference/ctime/strftime/)

[시간 구조체](https://www.cplusplus.com/reference/ctime/tm/)

[날짜 및 시간 처리 함수](https://www.cplusplus.com/reference/ctime/)
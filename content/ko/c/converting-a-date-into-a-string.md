---
title:    "C: 날짜를 문자열로 변환하기"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 왜
누군가는 날짜를 문자열로 변환하는 것을 왜 하지w 면서 궁금할 수 있습니다. 이 글에서 우리는 이러한 변환의 중요성과 이를 어떻게 할 수 있는지를 살펴보겠습니다.

## 어떻게 할까요
```C
#include <stdio.h>
#include <time.h>

int main(void){
    // 현재 시간을 구하는 함수인 time() 함수를 사용합니다.
    time_t t = time(NULL);
    // 관습적인 형식인 RFC 822 형식으로 시간을 변환합니다.
    char buffer[26];
    strftime(buffer, 26, "%a, %d %b %Y %H:%M:%S %z", localtime(&t));
    // 결과를 출력합니다.
    printf("오늘은 %s 입니다.\n", buffer);
    return 0;
}
```
```bash
> 오늘은 Thu, 02 Sep 2021 17:49:35 +0900 입니다.
```

프로그래밍 언어 C에서 날짜와 시간은 시스템에서 제공하는 시간 함수를 사용하여 관리됩니다. 그 중에서도 `time()` 함수는 주로 현재 시간을 구하는데 사용됩니다. 이 함수를 이용해서 얻어낸 시간은 숫자 형태이기 때문에 보통 사람이 읽기 쉬운 형태인 문자열로 변환하게 됩니다. 이때, `strftime()` 함수를 사용하여 원하는 형식으로 변환할 수 있습니다. 이 함수는 다양한 형식의 문자열을 제공하며, 직접 형식을 지정해줄 수도 있습니다. 위 코드에서는 RFC 822 형식을 사용한 예시를 보여줍니다.

## 깊이 파보기
프로그래밍에서 날짜와 시간은 아주 중요한 요소 중 하나입니다. 이를 변환하는 방법을 알아보는 것만으로도 많은 것을 배울 수 있습니다. 날짜와 시간은 시스템에서 제공하는 시간 함수를 이용하여 얻어낼 수 있으며, 문자열로 변환하고 싶다면 `strftime()` 함수를 사용하면 됩니다. 이 함수를 더 자세히 알기 위해서는 날짜와 시간에 관한 더 깊은 지식이 필요합니다. 이를 확장하고 싶다면 다른 프로그래밍 언어에서도 제공하는 시간 함수를 살펴보며 비교해보는 것도 좋을 것 같습니다.

# 참고
## 링크들
1. [C언어 날짜와 시간 함수 - 나무위키](https://namu.wiki/w/C%EC%96%B8%EC%96%B4%20%EB%82%A0%EC%A7%9C%EC%99%80%20%EC%8B%9C%EA%B0%84%20%ED%95%A8%EC%88%98)
2. [C 날짜와 시간 처리 - GeeksforGeeks](https://www.geeksforgeeks.org/c-date-time-processing/)
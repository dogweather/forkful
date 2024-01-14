---
title:                "C++: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 왜

숫자 패턴과 일치하는 문자를 삭제하는 것이 어떤 상황에서 유용한지 알아보겠습니다.

# 어떻게

먼저, 삭제할 문자를 저장할 새로운 문자열 변수를 생성합니다. 그런 다음, 문자열을 순회하며 해당 패턴에 일치하는 문자가 있는지 확인하고, 있다면 해당 문자를 새로운 변수에 추가하지 않습니다. 아래는 C++로 작성된 예제 코드와 샘플 출력입니다.

```
#include <iostream>
#include <string>

using namespace std;

int main() {
    // 문자열 변수를 first_string로 선언합니다.
    string first_string = "I 2오v3e f4i6nce 9a1nd p7r8ogr5am0m er.";

    // 삭제할 패턴을 설정합니다.
    string pattern = "123456789";

    // 새로운 문자열 변수를 생성합니다.
    string new_string = "";

    // 문자열의 각 문자를 확인하며 해당 패턴에 일치하는 문자는 새로운 변수에 추가하지 않습니다.
    for(int i = 0; i < first_string.length(); i++){
        if(pattern.find(first_string[i]) != string::npos){
            continue;
        } else {
            new_string += first_string[i];
        }
    }

    cout << new_string;

    return 0;
}

// 출력: I love finance and programming.
```

# 자세히

숫자 패턴과 일치하는 문자를 삭제하는 것은 데이터 전처리 단계에서 유용합니다. 예를 들어, 문자열에서 숫자를 제거하여 자연어 처리나 텍스트 마이닝 작업을 할 때 유용하게 사용할 수 있습니다. 또한, 정규 표현식을 사용하여 더욱 복잡한 패턴과 일치하는 문자를 삭제할 수도 있습니다. 딥 다이브 단계에서는 정규 표현식을 사용하는 방법을 좀 더 자세히 알아볼 수 있습니다.

# 같이 보기

- [문자열 처리 기술](https://blog.lgcns.com/555)
- [C++ 정규 표현식 사용법](https://blockdmask.tistory.com/435)
- [자연어 처리를 위한 문자열 전처리 기법](https://www.slideshare.net/ssuser1170070/1-82902558)
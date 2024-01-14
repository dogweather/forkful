---
title:    "C++: 정규 표현식 사용하기"
keywords: ["C++"]
---

{{< edit_this_page >}}

# 왜 정규 표현식을 사용해야 하는가?
정규 표현식은 특정 패턴의 문자열을 검색하고 추출하는 매우 유용한 도구입니다. 이를 통해 코드를 효율적으로 작성하고 코드의 일관성을 유지할 수 있습니다.

## 사용 방법
정규 표현식을 사용하기 위해서는 <regex> 헤더 파일을 포함해야 합니다. 그리고 regex 객체를 생성하여 검색 패턴을 정의하고, 더 나은 코드 가독성을 위해 문자열 리터럴 앞에는 'R'을 붙이는 것이 일반적입니다.

```C++
#include <iostream>
#include <string>
#include <regex>

using namespace std;

int main () {
    string str = "안녕하세요, 제 이름은 John입니다.";
    regex reg_name (R"(John)");
    smatch match;
    
    if (regex_search(str, match, reg_name)) {
        cout << match[0] << endl;
    }
    return 0;
}

// Output: John
```

위의 예제에서는 문자열 "안녕하세요, 제 이름은 John입니다."에서 "John" 패턴을 검색하고 해당 부분을 출력하는 간단한 코드입니다. 이처럼 정규 표현식을 사용하면 복잡한 문자열 처리를 간편하게 할 수 있습니다.

## 깊이 있는 정보
정규 표현식 패턴에는 다양한 특수 문자가 존재하여 각각의 의미를 잘 알아야 합니다. 예를 들어, "." 문자는 어떤 문자든지 1글자를 의미하는 와일드카드로 사용되지만, "\d"는 숫자를 의미합니다. 또한 정규 표현식 패턴에는 여러 개의 일치하는 문자열이 존재할 수 있을 때 어떤 패턴에 우선 순위를 부여할 것인지에 대한 기준도 필요합니다.

# 참고 자료
- [C++ 정규 표현식 레퍼런스](https://en.cppreference.com/w/cpp/regex)
- [정규 표현식 패턴 정리](https://www.fileformat.info/tool/regex.htm)
- [정규 표현식 실습 사이트](https://regexr.com/)
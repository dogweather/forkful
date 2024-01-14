---
title:    "C: 서브스트링 추출"
keywords: ["C"]
---

{{< edit_this_page >}}

## 왜

서브스트링 추출을 하는 이유는 기존의 문자열에서 일부분을 추출하여 다른 형식으로 사용할 수 있기 때문입니다. 예를 들어, 전화번호를 입력받을 때 하이픈이 포함된 문자열을 추출하여 번호만 가져올 수 있습니다.

## 방법

서브스트링을 추출하는 방법은 간단합니다. 우선, 추출하고자 하는 문자열의 시작 위치와 길이를 지정해야 합니다. 다음은 C 프로그래밍 언어를 사용하여 서브스트링을 추출하는 간단한 예시입니다.

```C
#include <stdio.h>
#include <string.h>

int main() {
    // 서브스트링을 추출할 문자열
    char str[] = "Hello World!";
    // 추출할 시작 위치
    int start = 6;
    // 추출할 길이
    int length = 5;
    // 추출할 서브스트링을 저장할 버퍼
    char substr[6];

    // str에서 start 위치부터 length 길이만큼 문자를 추출하여 substr에 저장
    strncpy(substr, str + start, length);

    // 출력
    printf("%s\n", substr);  // World

    return 0;
}
```

위 코드에서 `strncpy` 함수를 사용하여 `str`에서 `start` 위치부터 `length` 길이만큼의 문자를 `substr`에 저장하였습니다. 이렇게 하면 `substr`에는 `%s`를 사용하여 출력할 수 있는 서브스트링이 저장됩니다.

## 딥 다이브

서브스트링을 추출하는 방법은 `strncpy` 함수 외에도 여러 가지가 있습니다. 예를 들어, `strchr` 함수를 사용하면 문자열에서 특정 문자의 위치를 찾을 수 있습니다. 이를 이용해 서브스트링의 시작 위치를 찾아 `strncpy` 함수로 추출하는 것도 가능합니다.

또한, 서브스트링 추출은 문자의 대소문자를 구분하기 때문에 `strncasecmp` 함수를 사용하면 대소문자를 무시하고 서브스트링을 추출할 수도 있습니다.

## 또 다른 참고 자료

- [C 프로그래밍 언어 도서](https://www.hanbit.co.kr/store/books/category_list.html?cate_cd=001)
- [C 언어를 위한 프로그래밍 입문](http://www.tcpschool.com/c/)
- [C 프로그래밍 언어 공식 문서](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf)

---

## 참고하세요

- [서브스트링(Substring)](https://ko.wikipedia.org/wiki/%EC%84%9C%EB%B8%8C%EC%8A%A4%ED%8A%B8%EB%A7%81)
- [strncpy 함수](https://www.cplusplus.com/reference/cstring/strncpy/)
- [strchr 함수](https://www.cplusplus.com/reference/cstring/strchr/)
- [strncasecmp 함수](https://www.cplusplus.com/reference/cstring/strncasecmp/)
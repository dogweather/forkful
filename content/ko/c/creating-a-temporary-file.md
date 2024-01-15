---
title:                "임시 파일 만들기"
html_title:           "C: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

임시 파일을 만드는 이유는 자신이 사용하고 있는 메인 파일로부터 데이터를 임시로 저장하거나 처리하기 위해서입니다. 이 때, 임시 파일을 사용하면 메인 파일이 손상되거나 수정되는 것을 방지할 수 있습니다.

## 어떻게

```C
#include <stdio.h>
#include <stdlib.h>

int main(){
    // 임시 파일 만들기
    FILE * temp = tmpfile();

    // 임시 파일에 데이터 쓰기
    fprintf(temp, "Hello, world!");

    // 임시 파일 읽기
    rewind(temp);
    char buffer[30];
    fgets(buffer, 30, temp);

    // 출력: Hello, world!
    printf("%s\n", buffer);

    // 임시 파일 닫기
    fclose(temp);

    return 0;
}
```

## 더 들어가기

임시 파일은 프로그래밍에서 자주 사용되는 개념 중 하나입니다. 이는 파일 입출력을 통해 메모리를 임시로 할당하고, 이를 사용하여 데이터를 다루는 방식입니다. 임시 파일은 프로그램 실행 중에만 존재하며, 프로그램이 종료되면 자동으로 삭제됩니다.

## 더찾아보기

- [C 언어 임시 파일 생성하기](https://modoocode.com/117)
- [Linux 임시 파일 생성 방법](https://www.tecmint.com/create-temporary-file-in-linux/)
- [임시 파일이란?](https://blog.naver.com/PostView.nhn?blogId=itperson&logNo=221950736711&categoryNo=53&parentCategoryNo=0&viewDate=&currentPage=1&postListTopCurrentPage=1&from=postView)
- [C 언어 기초 개념: 임시 파일](https://12bme.tistory.com/80)
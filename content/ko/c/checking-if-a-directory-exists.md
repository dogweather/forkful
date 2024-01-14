---
title:                "C: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜
파일이나 디렉토리가 존재하는지 확인하는 것은 프로그래밍에서 매우 중요한 일입니다. 만약 어떤 파일이 존재하지 않는다면, 그 파일을 다루기 위해 추가적인 작업이 필요할 수 있기 때문입니다.

## 하는 방법
```C
#include <stdio.h>
#include <dirent.h>

int main() {
    DIR *dir = opendir("./my_directory"); // my_directory라는 디렉토리를 연다
    if (dir) {
        printf("my_directory 디렉토리가 존재합니다.");
        closedir(dir); // 디렉토리를 닫는다
    } else {
        printf("my_directory 디렉토리가 존재하지 않습니다.");
    }
    return 0;
}
```

위 예제는 `opendir()` 함수를 사용하여 디렉토리를 열고, 그 디렉토리가 열어졌는지 확인하는 방법을 보여줍니다. 만약 디렉토리가 존재한다면, `opendir()` 함수는 해당 디렉토리의 포인터를 반환하고, 그렇지 않으면 `NULL`을 반환합니다. 따라서 디렉토리가 존재하는지 여부를 확인할 수 있습니다.

## 깊게 들어가기
`dirent.h`라는 헤더 파일에는 디렉토리를 다루기 위한 다양한 함수들이 선언되어 있습니다. 위에서 사용한 `opendir()` 함수 외에도 `readdir()` 함수를 사용하여 디렉토리 내의 파일과 디렉토리 목록을 얻을 수 있습니다. 또한 `closedir()` 함수를 사용하여 디렉토리를 닫을 수 있습니다.

디렉토리가 존재하는지 확인하는 것은 절대적으로 필요한 작업은 아닙니다. 그러나 파일을 다룰 때 디렉토리가 존재하는지 먼저 확인하면 실행 중 오류가 발생할 가능성이 줄어듭니다.

## 참고자료
- [C언어로 배우는 리눅스 프로그래밍 3장 디렉토리 액세스](http://www.ktword.co.kr/abbr_view.php?m_temp1=4582)
- [C programming language - Dirent.h header](https://www.programiz.com/c-programming/library-function/dirent.h)
- [How to check if a folder exists in C?](https://stackoverflow.com/questions/7430248/how-to-check-if-a-folder-exists-in-c)
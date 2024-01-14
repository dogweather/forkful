---
title:    "C: text 파일 읽기"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것에 대해 궁금한 이유는 무엇일까요? 텍스트 파일은 컴퓨터에서 가장 기본적인 형식으로 데이터를 저장하고 전달하는데 사용됩니다. 따라서 프로그래밍을 배우는 과정에서 텍스트 파일을 읽고 쓰는 방법은 필수적입니다. 또한 텍스트 파일을 읽는 것은 새로운 정보를 얻고 공부하는 좋은 방법이 될 수 있습니다.

## 어떻게

텍스트 파일을 읽는 것은 C 프로그래밍에서 중요한 부분입니다. 우리는 `fopen()` 함수를 사용하여 파일을 열고 `fgetc()` 함수를 사용하여 파일에서 문자를 읽을 수 있습니다. 아래의 예제 코드를 참고하세요.

```C
#include <stdio.h>

int main() {
    // 파일 열기
    FILE *fp;
    fp = fopen("sample.txt", "r");

    // 파일에서 문자 읽기
    int c;
    c = fgetc(fp);
    while (c != EOF) {
        printf("%c", c);
        c = fgetc(fp);
    }

    // 파일 닫기
    fclose(fp);
}
```

위의 코드는 `sample.txt` 파일을 읽어서 콘솔에 출력하는 간단한 예제입니다. 아래는 예제의 출력 결과입니다.

```
This is a sample text file.
It contains some text that we want to read.
```

이 외에도 `fgets()` 함수를 사용하여 파일에서 한 줄씩 문자열을 읽는 것도 가능합니다. 더 많은 파일 입출력 함수에 대해서는 관련된 자료들을 참고하시기 바랍니다.

## 깊이 파고들기

우리가 읽는 텍스트 파일은 실제로 바이트 열의 형태로 저장되어 있습니다. 그리고 이 바이트 열은 ASCII 또는 유니코드 문자로 해석될 수 있습니다. 따라서 문자를 읽을 때, 우리는 문자를 나타내는 바이트 수를 고려해야 합니다.

또한 파일을 열 때 우리는 `fopen()` 함수에서 파일 모드를 설정할 수 있습니다. `r` 모드는 파일을 읽기 전용으로 열고, `w` 모드는 파일을 쓰기 전용으로 열게 됩니다. 그리고 `a` 모드는 파일에 데이터를 추가하게 됩니다. 파일 입출력에서 모드 설정은 매우 중요하며, 잘못된 모드 설정은 파일을 손상시킬 수 있습니다.

## 참고 자료

- [C에서 파일 열기 - opentutorials.org](https://opentutorials.org/course/1750/9648)
- [C 파일 입출력 - TutorialsTeacher](https://www.tutorialsteacher.com/c/c-file-io-functions)
- [C 언어 기초 - Codeit](https://www.codeit.kr/courses/intro-to-c-language)
- [ASCII 표 - ascii.cl](http://www.asciitable.com/)
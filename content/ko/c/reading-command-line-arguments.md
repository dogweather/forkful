---
title:                "C: 컴퓨터 프로그래밍: 명령 줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍: 명령 줄 인수 읽기"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜
커맨드 라인 인수를 읽는 방법을 배우는 것은 효율적인 프로그래밍을 위해 필수적입니다.

## 방법
커맨드 라인 인수를 읽는 방법은 간단합니다. 다음과 같은 코드를 작성하면 됩니다.

```C
int main(int argc, char *argv[]) {
    // 커맨드 라인 인수 출력
    for (int i = 0; i < argc; i++) {
        printf("인수 %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

위 코드를 실행하면 인수 0에는 프로그램의 실행 경로가, 인수 1부터는 사용자가 입력한 인수들이 출력됩니다. 예를 들어, `./program argument1 argument2`와 같이 입력하면 인수 0에는 `./program`이, 인수 1에는 `argument1`이, 인수 2에는 `argument2`가 출력됩니다.

위 방법 말고도 `getopt` 함수를 사용하여 더 유연하게 인수를 읽을 수 있습니다.

## 심층 분석
커맨드 라인 인수는 사용자로부터 입력을 받아 프로그램을 실행할 때 매우 유용하게 사용됩니다. 개발자는 사용자가 입력한 인수들을 분석하여 프로그램의 동작을 다르게 조절할 수 있습니다. 또한, 커맨드 라인 인수를 사용하면 프로그램의 테스트를 더 쉽게 할 수 있습니다. 예를 들어, 프로그램을 실행할 때 다른 인수를 입력하면서 프로그램이 정상적으로 동작하는지 확인할 수 있습니다.

커맨드 라인 인수를 사용할 때 주의해야 할 점은 입력된 인수들의 타입을 정확하게 맞추는 것입니다. 예를 들어, `atoi` 함수를 사용하여 문자열 타입의 인수를 정수로 변환할 수 있지만, 입력된 인수가 정수가 아닌 경우 프로그램에서 오류가 발생할 수 있습니다. 따라서 개발자는 사용자가 입력할 수 있는 모든 경우를 예상하고 그에 맞는 예외처리를 해주어야 합니다.

## 참고 자료
- [C언어 커맨드 라인 인수 읽기](https://www.joinc.co.kr/w/Site/system_programing/File/File_IO#AEN853)
- [getopt 함수 정리](https://black-live.tistory.com/13)
- [커맨드 라인 인수 사용 예제](https://github.com/jhyeok0220/C_Language/blob/master/%5BSTD%5D03_C%EC%96%B8%EC%96%B4/%5BSTD%5D03_%EC%BB%A4%EB%A7%A8%EB%93%9C%20%EB%9D%BC%EC%9D%B8%20%EC%9D%B8%EC%88%98.c)

## 참고 자료 (See Also)
- [C언어 메모리 관리](https://www.joinc.co.kr/w/man/2/malloc)
- [데이터 타입 변환](https://modoocode.com/12)
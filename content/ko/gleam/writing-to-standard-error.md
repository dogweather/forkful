---
title:                "Gleam: 표준 에러에 쓰는 방법"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜
Stderr에 쓰기를 하는 이유는 프로그래밍 과정에서 디버깅에 유용하기 때문입니다. 예외 처리나 오류를 추적할 때, stderr에 출력 내용을 기록하면 문제를 해결하는 데 도움이 됩니다. 

## 어떻게
Gleam에서 stderr에 쓰기를 하는 방법은 매우 간단합니다. 우선 `gleam/io` 모듈을 import해야 합니다. 그 후 `stderr` 함수를 사용하여 원하는 문자열을 stderr에 쓸 수 있습니다. 아래는 예제 코드와 출력 결과입니다.

```Gleam
import gleam/io

let my_error = "에러가 발생했습니다."
stderr(my_error)
```
출력 결과:
```
에러가 발생했습니다.
```

## 딥 다이브
Gleam은 쓰기를 위한 오버로드된 함수인 `write`와 `writeln`도 제공합니다. `write` 함수는 문자열을 바로 쓸 수 있고, `writeln` 함수는 해당 문자열 뒤에 개행 문자를 추가하여 쓸 수 있습니다. 이러한 함수를 사용하면 더욱 효율적으로 stderr에 쓸 수 있습니다.

또한, stderr에 쓴 내용을 파일로 저장하는 방법도 있습니다. `std/file` 모듈을 import하여 `File` 타입의 인스턴스를 생성한 후 `std/file/write` 함수를 사용하여 stderr의 출력 내용을 파일로 저장할 수 있습니다.

## 참고
* [Gleam 공식 문서](https://gleam.run/book/getting-started/installation.html)
* [Gleam 표준 라이브러리 문서](https://gleam.run/lib/stdlib.html)
* [Gleam 커뮤니티 포럼](https://forum.gleam.run/)

See Also
## 더 알아보기
* [Gleam에서 stderr 사용하기](https://gleam.run/2019/04/22/using-stderr-in-gleam.html)
* [파일 입출력 관련 Gleam 예제 코드](https://github.com/technomancy/gleam/blob/master/lib/std/file.gleam)
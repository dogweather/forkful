---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:13.538226-07:00
description: "C\uC5D0\uC11C \uD45C\uC900 \uC624\uB958\uB85C \uC4F0\uAE30\uB294 \uC5D0\
  \uB7EC \uBA54\uC2DC\uC9C0\uC640 \uC9C4\uB2E8 \uC815\uBCF4\uB97C \uC8FC \uD504\uB85C\
  \uADF8\uB7A8 \uCD9C\uB825\uACFC \uB2E4\uB978 \uBCC4\uB3C4\uC758 \uC2A4\uD2B8\uB9BC\
  \uC73C\uB85C \uBCF4\uB0B4\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC624\uB958 \uBA54\uC2DC\uC9C0\uB97C \uD45C\
  \uC900 \uCD9C\uB825\uC73C\uB85C\uBD80\uD130 \uBD84\uB9AC\uD558\uC5EC \uB514\uBC84\
  \uAE45\uC774\uB098 \uD504\uB85C\uADF8\uB7A8 \uC2E4\uD589 \uB85C\uAE45 \uC2DC \uB354\
  \ \uC27D\uAC8C \uC77D\uACE0 \uCC98\uB9AC\uD560 \uC218 \uC788\uB3C4\uB85D \uC774\uB97C\
  \ \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.951678-06:00'
model: gpt-4-0125-preview
summary: "C\uC5D0\uC11C \uD45C\uC900 \uC624\uB958\uB85C \uC4F0\uAE30\uB294 \uC5D0\uB7EC\
  \ \uBA54\uC2DC\uC9C0\uC640 \uC9C4\uB2E8 \uC815\uBCF4\uB97C \uC8FC \uD504\uB85C\uADF8\
  \uB7A8 \uCD9C\uB825\uACFC \uB2E4\uB978 \uBCC4\uB3C4\uC758 \uC2A4\uD2B8\uB9BC\uC73C\
  \uB85C \uBCF4\uB0B4\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uC624\uB958 \uBA54\uC2DC\uC9C0\uB97C \uD45C\uC900\
  \ \uCD9C\uB825\uC73C\uB85C\uBD80\uD130 \uBD84\uB9AC\uD558\uC5EC \uB514\uBC84\uAE45\
  \uC774\uB098 \uD504\uB85C\uADF8\uB7A8 \uC2E4\uD589 \uB85C\uAE45 \uC2DC \uB354 \uC27D\
  \uAC8C \uC77D\uACE0 \uCC98\uB9AC\uD560 \uC218 \uC788\uB3C4\uB85D \uC774\uB97C \uC218\
  \uD589\uD569\uB2C8\uB2E4."
title: "\uD45C\uC900 \uC624\uB958\uC5D0 \uC4F0\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

C에서 표준 오류로 쓰기는 에러 메시지와 진단 정보를 주 프로그램 출력과 다른 별도의 스트림으로 보내는 것을 포함합니다. 프로그래머들은 오류 메시지를 표준 출력으로부터 분리하여 디버깅이나 프로그램 실행 로깅 시 더 쉽게 읽고 처리할 수 있도록 이를 수행합니다.

## 방법:

C에서는 `stderr` 스트림을 사용하여 에러 메시지를 씁니다. `printf`를 사용하여 표준 출력에 쓰는 것과 달리, `stderr`에 쓰기는 `fprintf` 또는 `fputs`를 사용할 수 있습니다. 다음은 방법입니다:

```c
#include <stdio.h>

int main() {
    fprintf(stderr, "이것은 에러 메시지입니다.\n");

    fputs("이것은 또 다른 에러 메시지입니다.\n", stderr);
    
    return 0;
}
```

샘플 출력 (stderr로):
```
이것은 에러 메시지입니다.
이것은 또 다른 에러 메시지입니다.
```

출력이 콘솔에서 `stdout`과 비슷해 보일 수 있지만, 터미널에서 리다이렉션을 사용할 때 차이점이 명확해집니다:

```sh
$ ./your_program > output.txt
```

이 명령은 표준 출력만 `output.txt`로 리다이렉션합니다. 에러 메시지는 여전히 화면에 표시됩니다.

## 심화 학습

Unix 기반 시스템에서 `stdout`과 `stderr`의 구분은 C와 Unix의 초기 날로 거슬러 올라갑니다. 이러한 분리는 프로그래머가 표준 프로그램 출력과 독립적으로 에러 메시지를 리다이렉션 할 수 있도록 하여 더 견고한 에러 처리와 로깅을 가능하게 합니다. `stderr`은 디버깅, 충돌 및 기타 중대한 문제에 대한 에러 메시지의 즉각적인 출력을 보장하기 위해 기본적으로 버퍼링되지 않지만, `stdout`은 일반적으로 버퍼링되어 있어 출력이 버퍼가 플러시될 때까지 지연될 수 있습니다(예: 프로그램 완료 또는 수동 플러시).

현대 애플리케이션에서는, 특히 명령줄 도구와 서버 애플리케이션에서 정규 로그 메시지와 오류를 구분하는 것이 중요하기 때문에, `stderr`로 쓰기는 여전히 관련이 있습니다. 그러나, GUI 애플리케이션에서 더 복잡한 오류 처리가 필요하거나 더 정교한 로깅 메커니즘이 필요한 경우, 프로그래머는 메시지 형식, 목적지(예: 파일, 네트워크), 그리고 심각도 수준(info, warning, error 등)을 제어할 수 있는 전용 로깅 라이브러리를 사용할 수 있습니다.

`stderr`은 C에서 오류 보고의 기본적인 메커니즘을 제공하지만, 프로그래밍 관행의 발전과 고급 로깅 프레임워크의 가능성은 종종 현대적인 오류 처리 전략의 출발점임을 의미합니다.

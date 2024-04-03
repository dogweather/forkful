---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:06:21.261852-07:00
description: "\uBC29\uBC95: C\uC5D0\uC11C\uB294 `main` \uD568\uC218\uB97C `int argc`\uC640\
  \ `char *argv[]` \uD30C\uB77C\uBBF8\uD130\uB97C \uC0AC\uC6A9\uD558\uC5EC \uCEE4\uB9E8\
  \uB4DC \uB77C\uC778 \uC778\uC790\uB97C \uBC1B\uB3C4\uB85D \uC124\uACC4\uD560 \uC218\
  \ \uC788\uC2B5\uB2C8\uB2E4. \uC5EC\uAE30\uC11C, `argc`\uB294 \uC804\uB2EC\uB41C\
  \ \uC778\uC790\uC758 \uC218\uB97C \uB098\uD0C0\uB0B4\uACE0, `argv`\uB294 \uBAA8\uB4E0\
  \ \uC778\uC790\uB97C \uB098\uC5F4\uD558\uB294 \uBB38\uC790 \uD3EC\uC778\uD130\uC758\
  \ \uBC30\uC5F4\uC785\uB2C8\uB2E4. \uB2E4\uC74C\uC740\u2026"
lastmod: '2024-03-13T22:44:55.950034-06:00'
model: gpt-4-0125-preview
summary: "C\uC5D0\uC11C\uB294 `main` \uD568\uC218\uB97C `int argc`\uC640 `char *argv[]`\
  \ \uD30C\uB77C\uBBF8\uD130\uB97C \uC0AC\uC6A9\uD558\uC5EC \uCEE4\uB9E8\uB4DC \uB77C\
  \uC778 \uC778\uC790\uB97C \uBC1B\uB3C4\uB85D \uC124\uACC4\uD560 \uC218 \uC788\uC2B5\
  \uB2C8\uB2E4."
title: "\uBA85\uB839 \uC904 \uC778\uC218 \uC77D\uAE30"
weight: 23
---

## 방법:
C에서는 `main` 함수를 `int argc`와 `char *argv[]` 파라미터를 사용하여 커맨드 라인 인자를 받도록 설계할 수 있습니다. 여기서, `argc`는 전달된 인자의 수를 나타내고, `argv`는 모든 인자를 나열하는 문자 포인터의 배열입니다. 다음은 이를 설명하는 간단한 예시입니다:

```c
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("프로그램 이름: %s\n", argv[0]);
    printf("인자의 수: %d\n", argc - 1);
    for (int i = 1; i < argc; i++) {
        printf("인자 %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

위 코드를 사용해서 프로그램이 `./programName -a example`로 실행된다면, 출력은 다음과 같을 것입니다:

```
프로그램 이름: ./programName
인자의 수: 2
인자 1: -a
인자 2: example
```

이는 커맨드 라인 인자가 C 프로그램에서 어떻게 구문 분석되고 사용될 수 있는지 보여줍니다.

## 심층 분석
프로그램에 인자를 전달하는 관습은 Unix의 초기 시절로 거슬러 올라갑니다. 이 전통적인 접근법에서, `argc`와 `argv`는 커맨드 라인 상호작용을 위한 단순하지만 강력한 인터페이스를 제공하며, 함께 작동하는 작고 모듈화된 유틸리티의 Unix 철학을 구현합니다. 현대 언어는 종종 커맨드 라인 인자를 구문 분석하기 위한 더 정교한 라이브러리나 프레임워크를 도입하지만, C의 방법론은 비교할 수 없는 투명성과 제어력을 제공합니다.

최근의 개발에서는, POSIX 시스템의 `getopt`와 같은 라이브러리가 장기적인 옵션 이름을 처리하거나 누락된 인자에 대한 기본값을 지원하는 것과 같은 더 복잡한 구문 분석 요구를 지원하기 위해 발전해 왔습니다. 그러나, `argc`와 `argv`의 기본 메커니즘은 C에서 프로그램이 실행 환경과 어떻게 상호작용하는지 이해하는 데 필수적입니다.

비판자들은 `argc`와 `argv`를 직접 다루는 것이 오류를 발생시킬 수 있다고 주장하며, 더 높은 수준의 추상화 사용을 촉진할 수 있습니다. 그럼에도 불구하고, C의 복잡성을 마스터하고 그것의 저수준 작동의 미묘함을 평가하려는 이들에게 커맨드 라인 인자 파싱을 마스터하는 것은 통과 의례입니다. 이 역사적인 방법론과 실용적인 유틸리티의 혼합은 시스템 프로그래밍과 소프트웨어 개발에서 C가 지속적으로 매력을 발산하는 많은 부분을 포함합니다.

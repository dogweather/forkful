---
title:                "Fish Shell: 정규 표현식을 사용하는 방법"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜
정규 표현식을 사용하게 되는 이유는 여러분이 프로그래밍을 할 때 특정한 패턴을 찾기 위해서 입니다. 예를 들어, 이메일 주소나 전화번호와 같은 데이터에서 원하는 정보를 추출하기 위해서 정규 표현식을 사용할 수 있습니다.

## 방법

```shell
Fish Shell에서 정규 표현식을 사용하는 방법은 매우 간단합니다. 먼저 "grep -E" 명령어를 사용하여 검색할 파일이나 텍스트를 지정합니다. 그리고 검색하려는 패턴을 작성합니다.

예를 들어, 이메일 주소를 찾는다면 다음과 같이 작성할 수 있습니다.

```Fish Shell
grep -E "[a-zA-Z0-9]+@[a-zA-Z0-9]+\.[a-zA-Z0-9]+" email.txt
```

위의 예제에서는 이메일 주소의 패턴을 작성하고 해당 파일에서 검색하도록 지정하였습니다. 간단한 예제이지만 정규 표현식을 잘 활용하면 더 복잡한 패턴을 찾을 수 있습니다.

## 깊이 파고들기

정규 표현식은 비록 처음 배우기엔 복잡해 보일지언정 사용하고 익히면 매우 효율적이고 강력한 도구입니다. 이를테면, 문자열에서 특정한 패턴을 찾을 때 와일드카드(wildcard) 문자인 *와 ?를 사용하면 매우 편리하게 패턴을 작성할 수 있습니다.

정규 표현식을 보다 자세히 공부하고 싶다면 다음 링크를 참고해보세요:

https://fishshell.com/docs/current/cmds/grep.html
https://www.regular-expressions.info/

## 참고 자료

다른 useful한 Fish Shell 명령어를 알고 싶다면 다음 링크들을 참고해보세요:

https://fishshell.com/docs/current/index.html
https://fishshell.com/docs/current/tutorial.html
---
title:                "Bash: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜 문자열 길이를 찾아야 할까요?

문자열 길이를 찾는 것은 프로그래밍에서 매우 중요한 부분입니다. 예를 들어, 어떤 문자열을 다룰 때, 우리는 그 문자열의 길이를 알아야만 해당 문자열을 올바르게 처리할 수 있습니다. 따라서 문자열의 길이를 찾는 것은 필수적입니다.

## 어떻게 하면 문자열 길이를 찾을 수 있을까요?

한 가지 간단한 방법은 Bash 내장 변수인 `$#`를 사용하는 것입니다. 이 변수는 현재 스크립트에 전달된 인수의 개수를 나타내는데, 하나의 문자열이 전달되었을 때 이 값은 해당 문자열의 길이와 동일합니다.

예제를 살펴보겠습니다.

```Bash
#!/bin/bash

# "Hello"라는 문자열을 사용하여 스크립트를 실행합니다.

echo "The length of the string is $#"
```

위 스크립트를 실행하면 다음과 같은 출력이 나옵니다.

```
The length of the string is 5
```

`$#` 변수를 사용하여 문자열의 길이를 찾을 수 있습니다. 하지만 이 방법은 문자열에 포함된 공백을 제외하고 길이를 계산하기 때문에, 정확한 길이를 알고 싶다면 다른 방법을 사용하는 것이 좋습니다.

`expr` 명령어를 사용하여 문자열의 길이를 센다는 것이 효율적인 방법입니다. 아래 예제를 살펴보세요.

```Bash
#!/bin/bash

# "Hello World"라는 문자열을 사용하여 스크립트를 실행합니다.

echo "The length of the string is $(expr length "Hello World")"
```

위 스크립트를 실행하면 다음과 같은 출력이 나옵니다.

```
The length of the string is 11
```

위 예제에서는 `expr length`를 사용하여 문자열의 길이를 계산했습니다. 따라서 이 방법을 사용하면 문자열의 공백도 포함하여 정확한 길이를 구할 수 있습니다.

## 깊이 들어가보면

Bash에서는 문자열의 길이를 찾는 다양한 방법이 있습니다. 예를 들면 `wc` 명령어를 사용할 수도 있습니다. 또한 `cut` 명령어를 사용해 원하는 부분만 추출한 후 그 길이를 구하는 것도 가능합니다. Bash에서 제공하는 다양한 내장 명령어와 함수를 사용하여 더욱 다양한 방법으로 문자열의 길이를 찾을 수 있습니다.

## 관련 사이트

- [Bash 공식 웹사이트](https://www.gnu.org/software/bash/)
- [Bash 입문자를 위한 튜토리얼](https://www.learnshell.org/en/)
- [Bash 스크립트 예제들](https://www.shellscript.sh/)
- [Linux 명령어 레퍼런스](https://www.linuxcommand.org/)
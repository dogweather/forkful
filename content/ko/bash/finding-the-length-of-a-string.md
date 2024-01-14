---
title:                "Bash: 문자열의 길이 찾기"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열 길이를 찾는 것에 참여하는 이유는 자동화 된 프로세스에서 문자열의 길이를 알아야하는 경우가 있기 때문입니다.

## 어떻게

"```Bash
# 문자열 길이를 찾는 간단한 예제
string="안녕하세요"
num=${#string}
echo "문자열의 길이는 $num입니다."
```

이 예제는 Bash에서 문자열 길이를 찾는 가장 간단하고 효율적인 방법을 보여줍니다. "$ {#variable}" 문법을 사용하면 변수의 길이를 찾을 수 있습니다. 

"```Bash
# 사용자 입력으로 문자열 길이를 찾는 예제
echo "문자열을 입력하세요:"
read string
num=${#string}
echo "입력한 문자열의 길이는 $num입니다."
```

이 예제는 사용자로부터 입력 받은 문자열의 길이를 찾는 방법을 보여줍니다. "read" 명령어를 사용하여 사용자 입력을 받고, 위에서 설명한 "$ {#variable}" 문법을 사용하여 입력 받은 문자열의 길이를 찾는 것입니다.

## 깊게 파고들기

원래는 Bash에서 문자열 길이를 찾는 것이 매우 간단하다는 것을 알았지만, 문자열 내에 첫 번째 문자가 공란인 경우에는 제대로 계산되지 않는다는 것을 발견했습니다. 

따라서 아래와 같이 "$ {variable%}" 문법을 사용하여 공란을 제거한 뒤, 다시 "$ {#variable}" 문법을 사용하여 변수의 길이를 찾아야 합니다.

"```Bash
# 공란을 제거한 뒤 문자열 길이를 찾는 예제
string=" hello world"
string=${string% }
num=${#string}
echo "공란을 제거한 문자열의 길이는 $num입니다."
```

위 예제에서는 원래 변수 "string"에 공란이 포함되어 있지만, "$ {string% }"를 사용하여 공란을 제거한 뒤 다시 길이를 찾는 것을 볼 수 있습니다.

## 또 다른 길이 찾기 방법

관련된 다른 유용한 정보를 알아보려면 아래 링크를 참조하시기 바랍니다.

### 더 많은 예제

- [Bash 문자열 길이 찾는 예제](https://www.howtogeek.com/275538/how-to-get-the-length-of-a-string-in-bash/)
- [Bash 문자열 가지고 놀기: 길이 찾기](https://linoxide.com/linux-shell-script/bash-string-length-count/)
- [Bash에서 문자열 처리하기](https://ryanstutorials.net/bash-scripting-tutorial/bash-strings.php)

## 더 많은 정보

위에 제시된 예제는 Bash에서 문자열 길이를 찾는 일반적인 방법을 보여주고 있습니다. 하지만 Bash에는 더 많은 문자열 처리와 관련된 유용한 기능이 있으며, 더 깊이 파고들어 공부해보시기 바랍니다.

## 참고자료

- [Bash 관련문자열 처리 관련 문서](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Shell-Parameter-Expansion)
- [Bash 쉘 스크립트 문서](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)
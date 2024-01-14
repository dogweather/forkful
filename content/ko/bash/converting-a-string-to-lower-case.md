---
title:    "Bash: 문자열을 소문자로 변환하기"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## 왜

쉘 스크립팅을 할 때 한 번쯤은 문자열을 소문자로 변환해야 할 때가 있습니다. 이를 하면 문자열을 비교하는 등의 작업에 도움이 되기 때문에 중요한 기술입니다.

## 방법

먼저, 소문자로 변환할 문자열을 선언합니다. 그런 다음 `tr` 명령어를 사용하여 소문자로 변환합니다.

```Bash
# 소문자로 변환할 문자열 선언
str="BLOG POST"

# tr 명령어를 사용하여 소문자로 변환
lowercase=$(echo $str | tr '[:upper:]' '[:lower:]')

# 변환된 문자열 출력
echo $lowercase
```

출력 결과는 다음과 같습니다.

```Bash
blog post
```

## 깊게 파고들기

`tr` 명령어는 입력으로 받은 문자를 다른 문자로 대체하는 기능을 합니다. 대체할 대상 문자들은 `[]` 안에 지정할 수 있으며, `[:upper:]`는 대문자, `[:lower:]`는 소문자를 의미합니다. 위 예시에서는 `[:upper:]`와 `[:lower:]`를 이용하여 모든 대문자를 소문자로 변환한 것을 볼 수 있습니다.

또한 `tr` 명령어의 또 다른 기능으로는 주어진 문자열에서 특정 문자를 삭제하는 것도 있습니다. 이를 이용하면 특정 문자를 소문자로 변환하는 것도 가능합니다. 예를 들어, `[:upper:]`와 `[:lower:]`를 제외한 모든 문자를 삭제할 수 있습니다.

## 관련 링크

- [Bash scripting tutorial](https://ryanstutorials.net/bash-scripting-tutorial/)
- [Linuxize: Using the tr Command in Linux](https://linuxize.com/post/how-to-use-the-tr-command-in-linux/)
- [GNU tr documentation](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
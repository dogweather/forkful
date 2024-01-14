---
title:    "Bash: 디렉토리가 존재하는지 확인하기"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

프로그래밍을 할 때, 특정 디렉토리가 존재하는지 확인하는 것은 중요한 일입니다. 이를 통해 디렉토리가 있는지 여부에 따라 코드를 다르게 실행할 수 있습니다.

## 방법

Bash에서 디렉토리의 존재 여부를 확인하는 가장 간단한 방법은 `test` 명령어를 사용하는 것입니다. 다음은 `test` 명령어를 사용해 디렉토리가 존재하는지 여부를 확인하는 예제 코드입니다.

```Bash
if test -d /path/to/directory; then
  echo "디렉토리가 존재합니다."
else
  echo "디렉토리가 존재하지 않습니다."
fi
```

위 코드에서 `-d` 옵션은 디렉토리가 존재하는지 여부를 확인하는데 사용됩니다. 이외에도 `-e`, `-f`, `-w` 등 다양한 옵션을 사용해 파일의 존재 여부를 확인할 수 있습니다.

## 깊게 파고들기

Bash에서 디렉토리 존재 여부를 확인하는데 있어 더 다양한 방법이 있습니다. 예를 들어 `&&`와 `||` 연산자를 사용해 디렉토리가 존재하는 경우와 존재하지 않는 경우에 따라 다른 코드를 실행할 수 있습니다. 또한 `if` 문이 아닌 `[]`를 사용하는 것도 가능합니다.

디렉토리 존재 여부를 확인하는 방법은 프로그래밍 언어에 따라 다소 차이가 있을 수 있으니 사용하고 있는 언어의 공식 문서를 참고하는 것이 좋습니다.

## 참고자료

- [Shell script if-then-else howto](https://bash.cyberciti.biz/guide/If_Statements)
- [Bash Scripting Tutorial for Beginners](https://linuxhandbook.com/bash-scripting-tutorial/)
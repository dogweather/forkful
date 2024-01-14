---
title:    "Bash: 문자열을 소문자로 변환하기"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##왜

문자열을 소문자로 변환하는 것에 대해 관심이 있는 이유는 무엇일까요? 이 작업은 많은 프로그래밍 언어에서 자주 사용되며 대소문자를 무시하고 비교할 때 유용합니다. 또한 대문자로 된 문자열을 소문자로 변환하여 데이터 정리 및 분석을 수행할 때도 유용합니다.

##어떻게

문자열을 소문자로 변환하는 방법은 여러 가지가 있지만 가장 간단하고 일반적인 방법은 Bash에서 제공하는 내장 함수 'tr'을 사용하는 것입니다. 아래는 간단한 예제 코드와 출력 예시입니다.

```Bash
# 문자열을 변수에 할당
my_string="Hello WORLD"

# tr 함수를 사용하여 문자열을 소문자로 변환
lowercase_string=$(echo $my_string | tr [A-Z] [a-z])

# 결과 출력
echo $lowercase_string
# 출력 예시: hello world
```

위 코드에서 사용된 '[A-Z]' 및 '[a-z]'는 대문자와 소문자의 범위를 나타냅니다. 따라서 'tr' 함수는 대문자를 소문자로 변환하는 역할을 합니다. 또 다른 방법으로는 'awk' 또는 'sed'와 같은 다른 Bash 명령어를 사용하여 문자열을 변환할 수도 있습니다.

##자세히 살펴보기

문자열을 소문자로 변환하는 작업은 대소문자를 처리하는 프로그래밍에서 중요한 역할을 합니다. 예를 들어 사람 이름, 이메일 주소 또는 파일 이름을 비교할 때 대소문자를 무시하는 것이 바람직한 경우가 많습니다. 또한 대문자로 된 문자열을 데이터 내에서 소문자로 일관되게 변환하면 데이터 정리 및 분석에 도움이 됩니다.

Bash에서 문자열을 소문자로 변환하는 것 외에도, 다른 프로그래밍 언어에서도 비슷한 기능을 제공합니다. 예를 들어 Python에서는 'lower()' 함수를 사용하여 문자열을 소문자로 변환할 수 있습니다.

##관련 정보

- 'tr' 함수에 대한 더 많은 정보는 [공식 Bash 문서](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html)를 참조하세요.
- Bash에서 제공하는 다른 문자열 변환 함수에 대한 정보는 [이 블로그 포스트](https://www.howtogeek.com/442422/using-the-tr-command-to-Lowercase-and-Uppercase-text-on-linux-using-bash/)를 참조하세요.
- Python에서 문자열을 변환하는 방법에 대한 자세한 내용은 [이 문서](https://docs.python.org/3.8/library/stdtypes.html#str.lower)를 참조하세요.

## 참고

- [GNU Bash 문서](https://www.gnu.org/software/bash/manual/html_node/index.html)
- [Bash Shell Scripting Tutorial](https://www.shellscript.sh/index.html)
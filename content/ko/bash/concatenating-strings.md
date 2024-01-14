---
title:                "Bash: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜 

문자열을 연결하는 것이 유용한 이유는 여러 개의 독립적인 문자열을 하나의 문자열로 합칠 수 있기 때문입니다. 이는 데이터 처리나 문자열 조작을 할 때 편리한 방법입니다. 

## 방법 

```Bash
# 변수에 문자열 할당
first_name="John"
last_name="Smith"

# 문자열 연결
full_name="$first_name $last_name"

# 출력
echo $full_name
```

출력:
```
John Smith
```

## 깊게 들어가기 

문자열을 연결할 때는 변수를 사용하거나 문자열을 따옴표로 묶어야 합니다. 그렇지 않으면 Bash는 각각의 문자열로 인식하여 원하는 결과를 얻을 수 없습니다. 또한 변수를 사용할 때는 `$` 기호를 붙여야 합니다. 

예를 들어, 다음과 같은 코드를 작성했을 때 

```
# 변수에 할당한 숫자 더하기
num1=10
num2=20
result=num1 + num2

# 출력
echo $result
```

출력은 다음과 같이 나올 것입니다. 

```
num1 + num2
```

이는 변수가 문자열로 연결되었기 때문입니다. 따라서 올바른 코드는 다음과 같이 작성되어야 합니다.

```
# 변수에 할당한 숫자 더하기
num1=10
num2=20
result=$((num1 + num2)) # 변수 뒤에 $ 기호 빠짐

# 출력
echo $result
```

출력:
```
30
```

## 관련 링크 

- [Bash 문자열 연산자](https://www.tldp.org/LDP/abs/html/string-manipulation.html)
- [Bash 쉘 스크립팅 가이드](https://www.linuxfordevices.com/tutorials/bash-scripting-guide)
- [Bash 공식 문서](https://www.gnu.org/software/bash/manual/bash.html) 

## 참고하기 

위에서 언급한 것처럼, Bash에서 문자열을 연결할 때는 변수를 사용하거나 따옴표를 사용해야 합니다. 그렇지 않으면 제대로 된 결과를 얻을 수 없습니다. 매우 중요한 지식이므로,Bash 프로그래밍을 할 때 항상 기억하고 적용해야 합니다.
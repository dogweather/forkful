---
title:                "Bash: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

# 왜

## 왜 substring을 추출하는 것이 중요한가요?

subtring 추출은 Bash 프로그래밍에서 매우 유용한 기능입니다. 예를 들어, 파일 이름에서 확장자를 제거하거나, 문자열에서 특정 부분만 추출하는 등의 작업을 할 수 있습니다. 이를 통해 자동화된 작업을 효율적으로 수행할 수 있으며, 코드를 간결하고 가독성 있게 만들 수 있습니다.

# 하는 방법

## 코드 예시

```Bash
# 파일 이름에서 확장자 추출
filename="example.txt"
extension="${filename##*.}"
echo $extension # 결과: txt
```

```Bash
# 다른 문자열에서 특정 부분만 추출
string="Hello, world!"
substring="${string:0:5}"
echo $substring # 결과: Hello
```

## 샘플 출력

위의 예시 코드를 실행한 결과는 다음과 같습니다.

```
txt
Hello
```

# 딥 다이브

## substring 추출의 원리

substring 추출은 변수에서 지정한 부분을 제거하고 남은 부분을 반환하는 것입니다. 이를 위해 다양한 방법을 사용할 수 있지만, 주로 변수 확장(parameter expansion)을 이용합니다. 위의 코드 예시에서 사용된 `${variable##pattern}`은 변수의 끝에서부터 지정한 pattern과 일치하는 부분을 모두 제거한 뒤 남은 값을 반환합니다.

## 더 많은 기능들

substring 추출에는 다양한 옵션이 있으며, 사용자가 원하는 상황에 맞게 적절한 방식을 선택할 수 있습니다. 예를 들어, 변수의 시작 부분에서부터 일치하는 패턴의 모든 부분을 제거하고 싶다면 `${variable#pattern}`을 사용할 수 있습니다. 더 자세한 정보는 Bash 공식 문서를 참고하시기 바랍니다.

# 관련 정보

## 관련 링크들

- [Bash 공식 문서](https://www.gnu.org/software/bash/manual/)
- [Bash substring 추출 예제](https://linuxhint.com/substring_in_bash/)
- [Linux 셸 스크립팅 튜토리얼](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)

# 이 문서는...

- 주제: substring 추출하는 방법
- 왜 이런 기능이 필요한지, 코드 예시와 샘플 출력, 그리고 딥 다이브와 관련 링크를 제공하여 Bash 프로그래밍에서 substring 추출이 어떻게 유용한지 알려주는 문서입니다.
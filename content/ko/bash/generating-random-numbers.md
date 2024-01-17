---
title:                "랜덤 숫자 생성하기"
html_title:           "Bash: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

난수 생성이란 무엇일까요? 그리고 프로그래머들이 이것을 왜 하는 걸까요? 난수 생성은 우리가 원하는 범위에서 랜덤하게 숫자를 생성하는 것을 말합니다. 프로그래머들은 이를 사용하여 다양한 알고리즘을 개발하고, 보안을 강화하며, 시스템 테스트를 진행하기 위해 사용합니다.

## 어떻게:

```bash
# 1부터 10까지 무작위 숫자 출력하기
echo $(( (RANDOM % 10) + 1))

# 문자열 리스트에서 랜덤한 값 출력하기
words=("apple" "banana" "orange")
echo "${words[RANDOM % ${#words[@]}]}"
```

### 샘플 출력:

6
banana

## 깊게 알아보기:

### 역사적인 배경:

난수 생성은 컴퓨터 과학과 수학에서 오랜 역사를 가지고 있습니다. 옛날 컴퓨터들은 하드웨어적으로 난수를 생성하는 방식을 사용하였지만, 현대의 컴퓨터에서는 소프트웨어적으로 난수를 생성합니다. 이는 더욱 정확하고 빠르게 무작위성을 제공해줍니다.

### 대안:

Bash 외에도 다른 언어들에서도 난수를 생성할 수 있습니다. 예를 들어, Python의 'random' 라이브러리를 사용할 수 있습니다. 또한, 난수 생성기 알고리즘도 다양하기 때문에, 프로그래머들은 알고리즘 선택에 대해 고려해야 합니다.

### 구현 세부 사항:

Bash에서는 '$RANDOM' 변수를 사용하여 난수를 생성할 수 있습니다. 이 변수는 32767까지의 정수를 생성하며, 이를 원하는 범위로 조정할 수 있습니다. 또한, 'shuf'와 같은 유틸리티를 사용하여 파일 내의 무작위 줄을 출력할 수도 있습니다.

## 관련 자료:

- [리눅스 기본 명령어들 에대한 정리](https://m.blog.naver.com/PostView.nhn?blogId=gygs566&logNo=110125626744&proxyReferer=https%3A%2F%2Fwww.google.com%2F)
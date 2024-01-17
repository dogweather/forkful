---
title:                "새 프로젝트 시작하기"
html_title:           "C: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "C"
category:             "C"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 무엇인고? 그래서 왜?

새 프로젝트를 시작하는 것은 단순히 새로운 코드를 작성하는 것입니다. 프로그래머들은 이를 통해 새로운 무언가를 만들고 동작 시키기 위해 코드를 구현하기 때문에 새로운 프로젝트를 시작합니다.

## 방법:

```C
#include <stdio.h>

int main(){
  printf("Hello world!");
  return 0;
}
```

예제 코드를 보면 알 수 있듯이 새 프로젝트를 시작하려면 ```main()``` 함수를 정의하고 해당 함수 내에 코드를 작성해야 합니다. 이 예제에서는 "Hello world!"라는 메시지를 출력하는 간단한 프로그램을 작성하였습니다. ```return 0;```은 함수의 종료를 나타내는 코드로, 이는 새로운 프로젝트를 실행하고 종료하기 위해 필요합니다.

## 깊이 파고들기:

새 프로젝트를 시작하는 아이디어는 1970년대부터 시작되었으며, 프로그래밍 언어의 발전과 함께 변화하였습니다. 현재 가장 널리 사용되는 언어 중 하나인 C 언어는 다양한 플랫폼에서 동작하는 언어로, 새 프로젝트를 시작하기에 최적화된 언어입니다. 또한 다른 언어와 연동되어 사용되기도 합니다.

## 추가로 읽어볼만한 것들:

- C 언어 자습서: https://www.tutorialspoint.com/cprogramming/
- C 언어 공식 문서: https://devdocs.io/c/
- C 프로젝트 예제: https://github.com/tuvtran/project-based-learning#c
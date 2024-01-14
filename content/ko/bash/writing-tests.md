---
title:                "Bash: 프로그래밍 테스트 작성"
simple_title:         "프로그래밍 테스트 작성"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

왜 코드를 작성할 때 테스트를 작성하는 것이 중요한지 궁금하신가요? 테스트를 작성하는 것은 버그를 방지하고 코드의 신뢰성을 높이는 중요한 과정입니다. 코드를 작성하기 전에 테스트를 작성해두면, 발생할 수 있는 버그를 미리 예방할 수 있으며, 코드를 변경하거나 업데이트할 때도 이전과 동일한 기능이 올바르게 작동하는지 확인할 수 있습니다.

## 하는 방법

테스트를 작성하는 가장 간단한 방법은 ```Bash``` 스크립트 안에 ```echo```를 사용하는 것입니다. 예를 들어, 다음과 같은 스크립트를 작성해보세요.

```Bash
#!/bin/bash

# 덧셈 함수 정의
sum() {
  return $(( $1 + $2 ))
}

# 함수 호출과 결과 출력
echo $(sum 3 4)
```

위의 스크립트를 실행하면, 결과로 ```7```이 출력될 것입니다. 이제, 테스트를 추가해보겠습니다. ```Bash``` 스크립트에서는 ```if```와 ```test``` 구문을 사용하여 조건을 확인할 수 있습니다. 이를 활용하여 다음과 같은 테스트를 추가하세요.

```Bash
#!/bin/bash

# 덧셈 함수 정의
sum() {
  return $(( $1 + $2 ))
}

# 함수 호출과 결과 출력
echo $(sum 3 4)

# 테스트
if [ $(sum 3 4) == 7 ]; then
  echo "테스트 성공!"
else
  echo "테스트 실패!"
fi
```

위 스크립트를 실행하면, "테스트 성공!"이 출력됩니다. 이와 같이, 간단한 테스트를 작성하여 코드의 결과를 확인할 수 있습니다.

## 깊게 들어가기

테스트를 작성하는 데에는 여러가지 방법이 있습니다. ```Bash``` 스크립트에서 자주 사용되는 ```curl```이나 ```wget```을 이용하여 서버의 응답을 검증하는 테스트를 작성할 수 있고, 특정 디렉토리나 파일이 존재하는지를 확인하는 등 다양한 테스트를 작성할 수 있습니다.

또한, ```Bash```에서는 ```test``` 외에도 ```if```, ```case```, ```while```, ```for``` 구문을 이용하여 더 복잡한 조건을 체크할 수 있습니다. 이를 활용하여 테스트를 작성할 수 있습니다. 또한, 변수나 함수 등을 이용하여 테스트 작성을 더욱 유연하게 할 수 있습니다.

## 해당 항목도 확인해보세요

* [Bash 공식 문서](https://www.gnu.org/software/bash/manual/bash.html)
* [John Bristowe의 "테스트가 무엇을 말하는가"](https://hbr.org/2012/07/what-testing-can-lead-to)
* [코드 테스트의 중요성 (글로벌 소프트웨어)](https://www.globalsoftwaresupport.com/importance-of-code-testing/)
* [쉘 스크립트 작성시 활용할만한 유용한 팁 (유튜브)](https://www.youtube.com/watch?v=oxuRxtrO2Ag)
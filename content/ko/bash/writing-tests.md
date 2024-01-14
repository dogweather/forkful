---
title:    "Bash: 프로그래밍 테스트 작성"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/bash/writing-tests.md"
---

{{< edit_this_page >}}

"## 왜 테스트를 작성해야 하는가?"

테스트 작성은 중요합니다. 우리가 처음부터 완벽한 코드를 작성할 수 없기 때문입니다. 따라서 테스트를 작성함으로써 버그를 미리 예방하고 코드의 안정성을 확보할 수 있습니다. 또한, 테스트를 작성함으로써 코드를 수정하거나 업데이트할 때 실수를 최소화할 수 있습니다.

"## 작성 방법"

테스트를 작성하는 가장 기본적인 방법은 `assert` 문을 이용하는 것입니다. 이 문장은 특정 조건이 참인지 검사하고, 만약 조건이 참이 아니라면 에러를 발생시킵니다. 아래는 간단한 숫자 계산을 테스트하는 예제 코드입니다.

```Bash
#!/bin/bash

# 숫자 더하기 함수
function add(){
  echo $(($1 + $2))
}

# 테스트 코드
assert $(add 2 3) -eq 5
assert $(add 5 10) -eq 15
assert $(add 0 0) -eq 0
```

위의 코드에서 `assert` 문은 각각의 `add` 함수 호출 결과가 예상한 값과 일치하는지를 검사합니다. 만약 일치하지 않는다면 스크립트는 에러를 발생시키고, 일치한다면 아무런 메시지도 출력하지 않습니다.

"## 더 깊게 알아보기"

테스트 작성에는 `assert` 문 외에도 다양한 기법과 도구들이 있습니다. 예를 들어, 리눅스 커널에는 `kselftest`라는 테스트 유틸리티가 내장되어 있어서, 리눅스 운영체제의 다양한 부분들을 자동으로 테스트할 수 있습니다. 또한, `bashunit`이나 `shunit2`와 같은 테스트 프레임워크를 이용하면 보다 구조적이고 효율적인 테스트를 작성할 수 있습니다.

"## 관련 자료"

- [assert 문서 (GNU Bash)](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html#Bash-Conditional-Expressions)
- [쉘 스크립팅 테스팅 (Mozilla Developer Network)](https://developer.mozilla.org/ko/docs/Learn/Scripting/Bash/Testing)
- [kselftest 사용 예제 (코드죽음을 부르는 방패)](https://kldp.org/node/63124)
- [bashunit (GitHub)](https://github.com/kward/shunit2)
- [shunit2 (GitHub)](https://github.com/rocky/bashunit)
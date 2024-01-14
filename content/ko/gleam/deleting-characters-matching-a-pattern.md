---
title:    "Gleam: 패턴과 일치하는 문자 삭제하기"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

왜: 패턴과 일치하는 문자를 삭제하는 것이 유용한 이유는 특정 데이터를 정리하고 필요한 정보를 추출하기 위해서입니다.

어떻게: 이제 Gleam 프로그래밍 언어를 사용하여 문자열에서 패턴에 일치하는 문자를 삭제하는 방법을 알아보겠습니다. 아래는 예시 코드와 출력 결과입니다. 

```Gleam 언어를 이용하여 패턴에 일치하는 문자를 삭제하는 예시 코드입니다.

// 문자열에서 a 문자를 포함하는 경우 해당 문자를 제거하는 함수
fn delete_a(input) {
  re = regex.new("a") // 정규 표현식으로 a를 찾습니다.
  regex.replace_all(input, re, "") // a를 빈 문자로 대체하여 삭제합니다.
}

// 예제 입력과 출력
input_str = "apple"
output_str = delete_a(input_str)
print(output_str) // "pple" 출력
```

깊이 파고들기: Gleam에서는 더 복잡한 패턴을 사용하여 문자를 삭제할 수도 있습니다. 예를 들어, 공백이나 특수 문자를 삭제하는 정규 표현식을 사용할 수 있습니다. 또한, 패턴을 사용하는 것 외에도 조건문을 추가하여 삭제할 문자를 세부적으로 선택할 수도 있습니다.

See Also:

- [Gleam 공식 문서](https://gleam.run/) 
- [Gleam 예제 코드 저장소](https://github.com/gleam-lang/gleam) 
- [정규 표현식 패턴 알아보기](https://www.rexegg.com/)
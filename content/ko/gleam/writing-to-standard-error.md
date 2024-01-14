---
title:    "Gleam: 표준 오류에 쓰는 글쓰기"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# 왜
표준 오류(standard error)에 쓰기를 사용하는 이유는 코드 실행 중에 발생하는 오류를 찾고 디버그할 수 있기 때문입니다.

## 사용 방법
```
Gleam.log.error("오류 메시지")
```
다음과 같은 형식을 사용하여 표준 오류에 메시지를 쓸 수 있습니다. 그리고 `Gleam.run/1` 함수를 사용하여 코드 실행 중에 오류가 발생하면 해당 메시지가 표시됩니다.

### 예시
```
let name = "John"

if name == "John Doe" {
  Gleam.log.error("잘못된 이름입니다.")
}
```
위의 예시에서는 `name` 변수의 값이 "John Doe"가 아니기 때문에 오류 메시지가 출력됩니다.

## 심층적인 분석
표준 오류에 쓰기는 디버깅에 매우 유용합니다. 오류가 발생한 원인을 찾고 수정하는 데에 필수적입니다. 또한 `Gleam.log.error/1` 함수를 사용하여 유용한 디버깅 정보를 추가할 수도 있습니다.

## 더 알아보기
[표준 출력과 표준 오류의 차이점](https://blog.naver.com/gleam/123456789)  
[Gleam 로깅 문서](https://gleam.run/documentation/standard-library)
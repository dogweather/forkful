---
title:    "Gleam: 정규식 사용하기"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

Gleam 프로그래밍 블로그 포스트: 정규식을 사용해보자

## 왜

정규식은 문자열에서 원하는 패턴을 찾거나 변형하는 데 유용하게 사용됩니다. 예를 들어, 전화번호를 입력받을 때 마스킹하여 보안성을 높이거나, 이메일 주소를 검증하는 등 다양한 상황에서 정규식은 중요한 역할을 합니다. 따라서 프로그래머라면 정규식에 대한 이해는 필수적입니다.

## 사용 방법

정규식을 Gleam에서 사용하려면 `Regex` 모듈을 import해야 합니다.

```Gleam
import Regex

```

그 후, `Regex.regex` 함수를 사용하여 패턴을 정의하고, `Regex.match` 함수를 사용하여 해당 패턴이 문자열과 일치하는지 확인할 수 있습니다. 예를 들어, 전화번호의 형식을 맞추기 위해 다음과 같은 코드를 작성할 수 있습니다.

```Gleam
let valid_phone_number = "^\\d{3}-\\d{4}-\\d{4}$"
let user_input = "010-1234-5678"

let match = Regex.match(valid_phone_number, user_input)

case match {
  Ok(_) -> io.print("올바른 전화번호입니다.")
  Error(_) -> io.print("잘못된 전화번호 형식입니다.")
}
```

위 코드를 실행하면 `올바른 전화번호입니다.`가 출력될 것입니다.

## 깊이 들어가기

정규식은 메타문자나 특수 문자를 사용하여 다양한 패턴을 정의할 수 있습니다. 예를 들어 `^`는 문자열의 시작을 의미하고, `$`는 문자열의 끝을 의미합니다. `\\d`는 숫자를 의미하고, `\\w`는 문자 또는 숫자를 의미합니다. 이 외에도 `?`, `*`, `+` 등 다양한 메타문자를 사용하여 패턴을 설정할 수 있습니다.

또한, 정규식에서 그룹을 사용하여 특정 부분을 뽑아낼 수 있습니다. 예를 들어, 전화번호의 형식 중에서 국가번호만 뽑아내고 싶다면 다음과 같이 코드를 작성할 수 있습니다.

```Gleam
let valid_phone_number = "^(\\d{3})-(\\d{4})-(\\d{4})$"
let user_input = "010-1234-5678"

let match = Regex.match(valid_phone_number, user_input)

case match {
  Ok(result) -> {
    let (_, country_code, _, _) = result
    io.print(country_code) // 010 출력
  }
  Error(_) -> ()
}
```

더 많은 메타문자나 그룹에 대한 정보는 [Gleam 문서](https://gleam.run/playground/regex)에서 확인할 수 있습니다.

## 참고 자료

- [Gleam 문서: 정규식](https://gleam.run/playground/regex)
- [정규식 빌더: RegExr](https://regexr.com/)
- [정규식 테스트 사이트: Regex Tester](https://regex101.com/)
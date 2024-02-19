---
aliases:
- /ko/rust/organizing-code-into-functions/
date: 2024-01-26 01:11:48.516552-07:00
description: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD558\uB294 \uAC83\
  \uC740 \uD504\uB85C\uADF8\uB7A8\uC744 \uC774\uB984\uC73C\uB85C \uC2DD\uBCC4\uB418\
  \uB294 \uC7AC\uC0AC\uC6A9 \uAC00\uB2A5\uD558\uACE0 \uBAA8\uB4C8\uC2DD\uC758 \uCCAD\
  \uD06C\uB85C \uB098\uB204\uB294 \uAC83\uC785\uB2C8\uB2E4. \uC6B0\uB9AC\uB294 \uCF54\
  \uB4DC\uB97C \uB354 \uAE68\uB057\uD558\uACE0, \uC77D\uAE30 \uC27D\uACE0, \uB514\uBC84\
  \uAE45\uD558\uAE30 \uC27D\uAC8C \uB9CC\uB4E4\uAE30 \uC704\uD574 \uC774\uB97C \uC218\
  \uD589\uD569\uB2C8\uB2E4. \uC774\uAC83\uC740 \uC6B0\uB9AC \uC790\uC2E0\uC744 \uBC18\
  \uBCF5\uD558\uC9C0 \uC54A\uACE0 \uC5C5\uB370\uC774\uD2B8\uB97C \uAC04\uC18C\uD654\
  \uD558\uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4."
lastmod: 2024-02-18 23:09:05.892631
model: gpt-4-1106-preview
summary: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD558\uB294 \uAC83\uC740\
  \ \uD504\uB85C\uADF8\uB7A8\uC744 \uC774\uB984\uC73C\uB85C \uC2DD\uBCC4\uB418\uB294\
  \ \uC7AC\uC0AC\uC6A9 \uAC00\uB2A5\uD558\uACE0 \uBAA8\uB4C8\uC2DD\uC758 \uCCAD\uD06C\
  \uB85C \uB098\uB204\uB294 \uAC83\uC785\uB2C8\uB2E4. \uC6B0\uB9AC\uB294 \uCF54\uB4DC\
  \uB97C \uB354 \uAE68\uB057\uD558\uACE0, \uC77D\uAE30 \uC27D\uACE0, \uB514\uBC84\uAE45\
  \uD558\uAE30 \uC27D\uAC8C \uB9CC\uB4E4\uAE30 \uC704\uD574 \uC774\uB97C \uC218\uD589\
  \uD569\uB2C8\uB2E4. \uC774\uAC83\uC740 \uC6B0\uB9AC \uC790\uC2E0\uC744 \uBC18\uBCF5\
  \uD558\uC9C0 \uC54A\uACE0 \uC5C5\uB370\uC774\uD2B8\uB97C \uAC04\uC18C\uD654\uD558\
  \uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4."
title: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇을 왜?
코드를 함수로 구성하는 것은 프로그램을 이름으로 식별되는 재사용 가능하고 모듈식의 청크로 나누는 것입니다. 우리는 코드를 더 깨끗하고, 읽기 쉽고, 디버깅하기 쉽게 만들기 위해 이를 수행합니다. 이것은 우리 자신을 반복하지 않고 업데이트를 간소화하는 것에 관한 것입니다.

## 방법:
원의 면적을 여러 번 계산하는 코드가 있다고 가정해 보겠습니다. 공식을 반복하는 대신 그것을 함수로 래핑합니다.

```Rust
fn calculate_circle_area(radius: f64) -> f64 {
    std::f64::consts::PI * radius.powi(2)
}

fn main() {
    let radius = 5.0;
    let area = calculate_circle_area(radius);
    println!("원의 면적은: {}", area);
}
```

출력:

```
원의 면적은: 78.53981633974483
```

## 심층 탐구
역사적으로, 함수는 입력을 출력으로 매핑하는 수학에서 왔습니다. 코딩에서는 어셈블리 시대부터 주변에 있었지만, 우리는 그것들을 '서브루틴'이라고 불렀습니다. 러스트의 함수는 값과 심지어 다른 함수들을 반환할 수 있습니다. 덕분에 일급 함수와 클로저가 있습니다.

대안? 인라인 코드나 매크로가 있지만, 복잡한 논리에 대해 엉망이 될 수 있습니다. 메소드가 있는 객체는 기능을 구성하는 또 다른 방법으로, 독립 함수에 비해 다른 맛을 띱니다.

러스트에서의 구현은 상당히 간단합니다. 함수는 그들의 매개변수 타입과 반환 타입을 선언합니다. 컨벤션에 의해 이름은 '스네이크 케이스'입니다. 모듈 외부에서 사용하기 위한 공개 함수(`pub fn`)와 내부 사용을 위한 비공개 함수가 있습니다. 그리고 러스트는 함수 내의 마지막 표현을 위해 `return` 키워드가 필요 없는 멋진 기능을 가지고 있습니다.

## 또한 보기
더 많은 정보를 위해 이것들을 확인하세요:
- 러스트 프로그래밍 언어 책: [함수](https://doc.rust-lang.org/book/ch03-03-how-functions-work.html)
- 예제로 배우는 러스트의 [함수](https://doc.rust-lang.org/rust-by-example/fn.html)

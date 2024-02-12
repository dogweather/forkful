---
title:                "리팩토링"
aliases: - /ko/swift/refactoring.md
date:                  2024-01-26T03:36:57.735533-07:00
model:                 gpt-4-0125-preview
simple_title:         "리팩토링"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/refactoring.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?
리팩토링은 기존의 컴퓨터 코드를 외부 동작을 변경하지 않고 구조를 재조정하는 과정입니다. 프로그래머들은 코드베이스를 정리하고 가독성, 유지보수성을 향상시키며, 최소한의 기술 부채로 미래 기능을 위한 길을 닦기 위해 이 작업을 진행합니다.

## 방법:
반복적인 코드가 있는 기본적인 Swift 예제부터 시작해 봅시다:

```Swift
func printUserDetails(firstName: String, lastName: String, age: Int) {
    print("First Name: \(firstName)")
    print("Last Name: \(lastName)")
    print("Age: \(age)")
}

func printUserJob(title: String, company: String) {
    print("Job Title: \(title)")
    print("Company: \(company)")
}
```

이를 리팩토링하려면 사용자 속성을 캡슐화하고 세부 정보를 출력하는 메소드를 추가한 `User` 구조체를 만드는 것을 포함합니다:

```Swift
struct User {
    let firstName: String
    let lastName: String
    let age: Int
    let jobTitle: String
    let company: String

    func printDetails() {
        print("First Name: \(firstName)")
        print("Last Name: \(lastName)")
        print("Age: \(age)")
        print("Job Title: \(jobTitle)")
        print("Company: \(company)")
    }
}

let user = User(firstName: "John", lastName: "Doe", age: 30, jobTitle: "Software Developer", company: "Tech Solutions")
user.printDetails()
```

### 샘플 출력:
```
First Name: John
Last Name: Doe
Age: 30
Job Title: Software Developer
Company: Tech Solutions
```

## 심층 분석
리팩토링은 소프트웨어 엔지니어링 초기부터 그 뿌리를 가지고 있지만, 특히 마틴 파울러의 주옥같은 책 "리팩토링: 기존 코드의 설계 개선"을 통해 1990년대 후반에 용어가 널리 퍼졌습니다. 이 책은 코드를 별도의 단계를 기다리지 않고 작은 단계로 지속적으로 정리해야 한다는 원칙을 제시했습니다.

수동 리팩토링의 대안으로는 중복 코드를 탐지하고, 단순화를 제안하며, 코드의 일부를 자동으로 생성할 수 있는 자동화 도구와 통합 개발 환경(IDE)이 포함됩니다. Swift 개발을 위한 Xcode는 이름 변경 및 메소드 추출 기능과 같은 다양한 리팩토링 도구를 제공하여, 프로세스에서 인간의 오류 가능성을 줄일 수 있습니다.

리팩토링을 구현할 때, 견고한 테스트 스위트를 갖추는 것이 중요합니다. 테스트는 안전망 역할을 하며, 변경 사항이 버그를 도입하지 않는지 확인합니다. 이는 리팩토링의 주된 목표가 외부 동작에 영향을 미치지 않으면서 내부 구조를 변경하는 것이기 때문에 필수적입니다.

## 참조:
- ["리팩토링: 기존 코드의 설계 개선" by 마틴 파울러](http://martinfowler.com/books/refactoring.html)
- [애플의 Swift 문서](https://swift.org/documentation/)
- [Xcode 리팩토링 도구 사용하기](https://help.apple.com/xcode/mac/current/#/dev91fe7130a)
- [레이 웬더리치의 Swift 스타일 가이드](https://github.com/raywenderlich/swift-style-guide)

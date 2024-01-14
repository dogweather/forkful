---
title:                "Gleam: 새로운 프로젝트를 시작하기"
programming_language: "Gleam"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜 
새로운 프로젝트를 시작하는 사람은 무엇을 기대할 수 있을까요? 새로운 언어를 배우고 싶거나, 다른 언어로 작성되어 있는 코드를 좀 더 효율적으로 관리하고 싶을 때 등 여러 이유로 새로운 프로젝트를 시작할 수 있습니다. Gleam 프로그래밍 언어를 배우는 것은 새로운 도전과 학습 경험이 될 것입니다.

## 어떻게 하나요
이제 실제로 Gleam 언어를 이용하여 새로운 프로젝트를 시작하는 방법에 대해 알아보겠습니다. 예제 코드와 함께 설명하도록 하겠습니다. 

```Gleam
pub struct Person {
  name: String,
  age: Int,
  gender: String,
}
```

위의 코드는 새로운 구조체를 정의하는 예제입니다. 다른 프로그래밍 언어에서는 클래스라는 개념을 많이 사용하지만, Gleam에서는 구조체를 이용하여 데이터를 구조화하고 관리할 수 있습니다. 이제 이 구조체를 이용하여 새로운 인스턴스를 만들어보겠습니다.

```Gleam
pub fn main() {
  let person = Person(name: "John", age: 30, gender: "Male")
  IO.puts("이름: " ++ person.name)
  IO.puts("나이: " ++ person.age)
  IO.puts("성별: " ++ person.gender)
}
```

위의 코드를 실행하면 “이름: John” “나이: 30” “성별: 남성” 문구가 출력될 것입니다. 물론 이는 간단한 예시일 뿐, Gleam에는 다양한 기능과 유용한 라이브러리들이 존재하며, 더 복잡하고 유용한 프로그램을 작성할 수 있습니다.

## 딥 다이브
새로운 프로젝트를 시작하는 것은 새로운 언어를 배우는 것과도 같습니다. Gleam 프로그래밍 언어의 경우 아직 많은 자료와 튜토리얼이 존재하지 않지만, 커뮤니티에서는 늘 활발히 활동하고 있으며 더 많은 자료와 정보를 통해 Gleam을 배울 수 있습니다.

더 자세한 내용은 공식 사이트를 참조하시거나 Gleam 커뮤니티에 참여하시면서 배우고 공유하는 것이 좋습니다.

## 더 알아보기
위에서 소개한 내용 외에도 Gleam 언어에 대한 더 많은 정보와 자료를 아래의 링크를 통해 확인하실 수 있습니다.

[공식 사이트](https://gleam.run/)
[Gleam 공식 튜토리얼](https://github.com/gleam-lang/gleam_tutorial)
[Gleam 커뮤니티 슬랙 채널](https://gleam-community.slack.com/)
[Gleam 공식 GitHub 저장소](https://github.com/gleam-lang/gleam)
[Gleam 커뮤니티 GitHub 저장소](https://github.com/search?q=topic:gleam&type=Repositories)
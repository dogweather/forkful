---
title:                "Gleam: 내용 cmdline 인수 읽기"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

많은 프로그래밍 언어들은 프로그램을 실행할 때 커맨드 라인에 입력된 인자들을 읽어와서 실행을 조정할 수 있도록 해줍니다. 그렇기 때문에 이번 블로그는 Gleam에서 커맨드 라인 인자를 어떻게 읽어오는지 알려드릴 것입니다.

## 어떻게

아래 코드 블록은 Gleam에서 커맨드 라인 인자를 읽어오는 방법을 보여줍니다. 이 코드를 실행할 경우, 터미널에서 입력한 숫자만큼 "Hello Gleam!" 메시지를 출력할 것입니다.

```Gleam
fn main(args) {
  // args의 타입은 [String],
  // 커맨드 라인 인자들의 리스트를 뜻합니다.
  let args_count = length(args)
  let num = String.to_integer(list.hd(args))
  for _ in 0..num {
    // for문 안의 코드들은 num만큼 반복됩니다.
    // args_count를 이용해서 프로그램이 몇 개의 인자를 받았는지 확인할 수 있습니다.
    let message = "Hello Gleam!"
    // println!는 Gleam에서 지원하는 출력 함수로,
    // message 변수의 값을 터미널에 출력합니다.
    println!(message)
  }
}
```

코드를 실행하기 전, 터미널에서 아래와 같이 커맨드 라인 인자를 추가해줘야 합니다.

```bash
gleam run main.gleam 5
```

위의 코드를 실행하면 "Hello Gleam!" 메시지가 5번 출력될 것입니다. 여러분도 위의 예제 코드를 바탕으로 커맨드 라인 인자를 읽어오는 방법을 익혀보세요!

## 딥 다이브

Gleam에서 커맨드 라인 인자를 읽어오기 위해 사용할 수 있는 라이브러리가 정말 다양합니다. 각 라이브러리는 각자의 장단점을 가지고 있기 때문에, 프로젝트에 맞는 가장 적합한 라이브러리를 골라 사용하는 것이 중요합니다. 자세한 내용은 아래의 "관련 링크"를 참고해주세요!

## 관련 링크

- [Gleam 공식 문서 - Reading Command Line Arguments](https://gleam.run/getting-started/reading-command-line-arguments/)
- [Gleam 라이브러리 - gleam/cli](https://github.com/gleam-lang/cli)
- [Gleam 라이브러리 - gleam/inquisitor](https://github.com/gleam-lang/inquisitor)

감사합니다!
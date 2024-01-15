---
title:                "디버그 출력 출력하기"
html_title:           "Rust: 디버그 출력 출력하기"
simple_title:         "디버그 출력 출력하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

코딩 중 디버그 출력하는 것이 왜 중요한지 궁금하지 않으신가요? 디버깅은 코드를 디버그하는 과정에서 필수적인 역할을 합니다. 디버그 출력은 러스트를 사용하는 개발자를 위한 유용한 도구입니다.

## 어떻게

디버깅을 위한 디버그 출력은 러스트에서 매우 쉽게 할 수 있습니다. 코드에서 ```println!()``` 함수를 사용하면 됩니다. 또한 ```dbg!()``` 매크로를 사용하여 변수의 값을 쉽게 확인할 수 있습니다.

```Rust
let var = 10;
println!("변수의 값: {}", var); // 출력: 변수의 값: 10
dbg!(var); // 출력: [src/main.rs:3] var = 10
```

### 디버그 출력 제어하기

디버그 출력은 기본적으로 활성화되어 있지만, 컴파일 시 debug 모드를 비활성화하여 제거할 수도 있습니다. 이렇게 하면 디버그 출력이 코드에 포함되지 않아 더 빠른 실행 속도를 얻을 수 있습니다.

```
$ cargo build --release
```

이 외에도 다양한 방식으로 디버그 출력을 제어할 수 있습니다. 자세한 내용은 러스트 공식 문서를 참고하세요.

### 프로그램 실행 중 상태 감시하기

디버깅용 디버그 출력 외에도, 프로그램 실행 중 상태를 감시해야 할 때도 디버그 출력이 유용합니다. 예를 들어 무한 반복문에서 중간 상황을 출력하여 프로그램의 진행 상황을 확인할 수 있습니다.

```Rust
let mut count: u32 = 0;

loop {
    dbg!(count);
    count += 1;
}
```

## 깊게 들어가기

디버그 출력은 매우 유용하지만, 너무 많이 사용하면 코드가 지저분해질 수 있습니다. 따라서 변수나 상태를 출력할 때는 필요한 부분만 선택적으로 출력하는 것이 좋습니다.

또한 ```debug!()``` 매크로를 사용하여 사용자 정의 타입에서 디버그 출력을 구현할 수도 있습니다. 이를 통해 사용자 정의 타입의 내부 값을 쉽게 확인할 수 있습니다.

## 이외에도

- [러스트 공식 문서](https://doc.rust-lang.org/std/macro.dbg.html)
- [디버그 출력을 활용한 디버깅](https://learnxinyminutes.com/docs/ko-kr/rust-ko/#%EB%94%94%EB%B2%84%EA%B9%85-debugging)
- [러스트 디버그 출력 강의](https://www.youtube.com/watch?v=GA1ZMF5OyKc)
---
title:                "임시 파일 만들기"
html_title:           "Javascript: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

임시 파일을 만드는 것은 일시적으로 사용되지만 필요한 파일로서, 개발자들이 자주 사용하는 기술입니다. 이는 주로 다른 파일들의 작업에서 임시적인 단계로 사용되고, 보통 프로그램이 끝나는 시점에 삭제됩니다. 이는 메모리나 디스크 공간을 효율적으로 사용하기 위해서 이해되어야 합니다.

## 하는 방법:

### 예제 1: 파일 이름 없이 임시 파일 만들기
```Javascript
const fs = require('fs');
const tmp = require('tmp');

const tmpobj = tmp.fileSync();
console.log('임시 파일:', tmpobj.name);
```

### 예제 2: 지정된 이름으로 임시 파일 만들기
```Javascript
const fs = require('fs');
const tmp = require('tmp');

const tmpobj = tmp.fileSync({ prefix: 'mytempfile-' });
console.log('임시 파일 이름:', tmpobj.name);
```

### 예제 1 결과:
임시 파일: C:\Users\Username\AppData\Local\Temp\tmp-x9m8NJLQ7WqjZ6bC

### 예제 2 결과:
임시 파일 이름: C:\Users\Username\AppData\Local\Temp\mytempfile-x9m8NJLQ7WqjZ6bC

*위의 예제는 Node.js 환경에서 실행되는 예제이며, 해당 라이브러리를 설치해야 정상적으로 작동합니다.

## 깊이 파고들기:

- 역사적 문맥: 임시 파일은 컴퓨터 시스템에서의 파일 관리 방법 중 하나로, 메모리나 디스크 용량의 한계로 인해 임시적인 파일이 필요해졌기 때문에 만들어졌습니다.
- 대안: 메모리나 디스크 용량이 충분한 시스템에서는 임시 파일을 따로 만들지 않고, 메모리 나 자체 데이터 구조를 사용할 수 있습니다.
- 구현 세부 사항: 임시 파일을 만드는 방법은 언어나 라이브러리에 따라 다를 수 있지만, 대부분 파일 시스템 명령어를 이용해 파일을 생성하고 삭제하는 방식으로 구현됩니다.

## 연관 자료:

- [Node.js tmp 모듈 문서](https://www.npmjs.com/package/tmp)
- [OS에서 임시 파일 만들기](https://www.lifewire.com/what-is-a-temporary-file-2619617)
- [임시 파일 만들기의 역사](https://blog.dominodatalab.com/la-vida-temporal-making-temporary-files-nodes-tmp-module/)
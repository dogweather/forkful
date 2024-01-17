---
title:                "임시 파일 만들기"
html_title:           "TypeScript: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

#

"## 무엇 & 왜?"

임시 파일을 만드는 것은 일시적으로 사용되는 파일이며, 프로그래머는 보통 이를 사용하여 프로세스 간 데이터를 공유하거나 임시로 저장하기 위해 사용합니다.

"## 방법:"

TypeScript 코드 블록 내에서 코딩 예제와 샘플 출력입니다.

```
// 임시 파일을 생성합니다.
import { tmpfile } from "fs";

const myTempFile = tmpfile();
console.log(myTempFile.name); // 임시 파일의 경로를 출력합니다.
```

```
// 임시 파일을 생성하고 해당 파일에 내용을 작성합니다.
import { writeFileSync } from "fs";

const myTempFile = tmpfile();
const content = "This is a temporary file.";
writeFileSync(myTempFile.name, content);

console.log(myTempFile.name); // 임시 파일의 경로를 출력합니다.
```

"## 더 들어가기:"

임시 파일을 생성하는 것은 과거에는 메모리를 절약하기 위해 더 흔했으며, 현재는 파일 시스템의 공간을 이용하기 위해 사용됩니다. 다른 방식으로는 파일을 여러 번 생성하고 삭제하는 것이 있지만, 이는 성능이 떨어질 수 있습니다. 실제로 임시 파일을 만드는 방식은 운영체제마다 다를 수 있습니다.

"## 참고자료:"

- [Tempfile module in Node.js](https://nodejs.org/api/fs.html#fs_fs_tmpfile_options_callback)
- [Using temporary files in TypeScript](https://www.geeksforgeeks.org/using-temporary-files-in-typescript/)
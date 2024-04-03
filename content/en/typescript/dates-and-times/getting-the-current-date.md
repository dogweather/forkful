---
date: 2024-02-03 19:02:32.159644-07:00
description: "How to: In TypeScript, you can use the `Date` object to get the current\
  \ date and time. Here\u2019s how you can do it."
lastmod: '2024-03-13T22:44:59.868109-06:00'
model: gpt-4-0125-preview
summary: In TypeScript, you can use the `Date` object to get the current date and
  time.
title: Getting the current date
weight: 29
---

## How to:
In TypeScript, you can use the `Date` object to get the current date and time. Here’s how you can do it:

```typescript
const currentDate = new Date();
console.log(currentDate);
```

Sample output:
```
2023-04-12T07:20:50.52Z
```

This code snippet creates a new `Date` object containing the current date and time, which is then printed to the console. You can also format the date using toLocaleDateString() for more readable formats:

```typescript
const currentDate = new Date();
console.log(currentDate.toLocaleDateString());
```

Sample output:
```
4/12/2023
```

### Using date-fns
For more extensive date manipulation and formatting, the `date-fns` library is a popular choice. First, install it via npm:

```bash
npm install date-fns
```

Then, you can use it to format the current date:

```typescript
import { format } from 'date-fns';

const currentDate = new Date();
console.log(format(currentDate, 'yyyy-MM-dd'));
```

Sample output:
```
2023-04-12
```

This `date-fns` example formats the current date as a string in "YYYY-MM-DD" format. The library offers a plethora of functions for date manipulation, making it a versatile tool for any TypeScript programmer working with dates.

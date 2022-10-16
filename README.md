# tomatobot

My first Haskell web app: a GroupMe bot which could, hypothetically, be used to deliver on-demand random tomato images to a GroupMe chat, _but which should, of course, only be used when adapted to follow the terms of service of the [Unsplash API](https://unsplash.com/developers)_

Best practices meet a best-effort development cycle:
I wrote this in one day, following best practices when I knew about them and when it wasn't too much work.

I structured the app with the [ReaderT design pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/), and, as always, adhered to the tenets of [type-driven design](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/).

Things to do differently next time:

- Use [Servant](https://docs.servant.dev/en/stable/) instead of WAI + Warp
- Don't commit hardcoded access tokens (they are no longer valid)
- Don't give up so early on error handling

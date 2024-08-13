import type { Site, SocialObjects } from "./types";

export const SITE: Site = {
  website: "https://blog.ccbhj.com/", // replace this with your deployed domain
  author: "ccbhj",
  profile: "https://github.com/ccbhj",
  desc: "my personal blog site",
  title: "ccbhj's blog",
  ogImage: "astropaper-og.jpg",
  lightAndDarkMode: true,
  postPerIndex: 4,
  postPerPage: 10,
  scheduledPostMargin: 15 * 60 * 1000, // 15 minutes
};

export const LOCALE = {
  lang: "en", // html lang code. Set this empty and default will be "en"
  langTag: ["en-EN"], // BCP 47 Language Tags. Set this empty [] to use the environment default
} as const;

export const LOGO_IMAGE = {
  enable: false,
  svg: true,
  width: 216,
  height: 46,
};

export const SOCIALS: SocialObjects = [
  {
    name: "Github",
    href: "https://github.com/ccbhj",
    linkTitle: ` ${SITE.title} on Github`,
    active: true,
  },
  {
    name: "Reddit",
    href: "https://www.reddit.com/user/dr1ft101/",
    linkTitle: `${SITE.title} on Reddit`,
    active: true,
  },
];

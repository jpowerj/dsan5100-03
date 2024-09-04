window.document.addEventListener("DOMContentLoaded", function (event) {
  var roomHtml = "<span class='sidebar-subtitle'>Tues 6:30-9pm, Car Barn 309</span>";
  var termHtml = "<span class='sidebar-subtitle'>Georgetown Fall 2024</span>";
  var emailHtml = "<span class='sidebar-subtitle w-100'>Prof. Jeff Jacobs&nbsp;<a href='mailto:jj1088@georgetown.edu' target='_blank'><i class='bi bi-envelope-at ps-1 pe-1'></i></a></span>";
  // https://icons8.com/icon/set/zoom/group-color
  var zoomIconBlue = "<svg xmlns='http://www.w3.org/2000/svg' x='0px' y='0px' width='22' height='22' viewBox='0 0 48 48'><circle cx='24' cy='24' r='20' fill='#2196f3'></circle><path fill='#fff' d='M29,31H14c-1.657,0-3-1.343-3-3V17h15c1.657,0,3,1.343,3,3V31z'></path><polygon fill='#fff' points='37,31 31,27 31,21 37,17'></polygon></svg>";
  var zoomHtml = `<a href='https://georgetown.zoom.us/j/92478089466?pwd=cSBmcjACL6EJJW0vHbzIqimH7imkVx.1' target='_blank' style='text-decoration: none !important;'><div class='sidebar-subtitle btn btn-outline-dark btn-block mt-2 mx-0 px-2 w-100 d-flex justify-content-center align-items-center'><span class='icon ps-0'>${zoomIconBlue}</span><div class='flex-grow-1 h-100' style='line-height: 1.0;'>Zoom Link</div><span class='bi bi-box-arrow-up-right pe-1 ps-1 me-0' style='font-size: 95%;'></span></div></a>`;
  var subtitleDiv = `<br>${roomHtml}<br>${termHtml}<br>${emailHtml}`;
  var inclZoom = false;
  if (inclZoom) {
    subtitleDiv = subtitleDiv + `<br>${zoomHtml}`;
  }
  $('.sidebar-title').append(subtitleDiv);
});
